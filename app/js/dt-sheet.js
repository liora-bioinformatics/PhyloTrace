/**
 * Spreadsheet-style range selection, clipboard and keyboard editing on top of a
 * DataTables grid that keeps DT's own `editable` cell editor.
 *
 * The selection lives in display coordinates (row position in the drawn tbody,
 * position among the visible columns) and is only translated to data
 * coordinates when an action runs, so sorting, filtering and the full redraw
 * every replaceData() causes never desync it - each `draw` simply repaints it.
 *
 * Nothing here writes cell data. Paste, cut, delete and fill hand Shiny one
 * batch of {isolate, col, value} edits, keyed by the row's column-0 value
 * rather than its position, and the server's validation (read-only columns,
 * dates, custom-variable types) stays the single authority on what is stored.
 * Undo/redo is server-side too (app/logic/sheet_grid.R keeps the history), so
 * it covers single-cell edits made in DT's own editor as well. The table
 * catches up through the server's own replaceData().
 */

var SEL = "sheet-sel";
var ACTIVE = "sheet-active";
// The range's bottom-right cell, which carries the fill handle (main.scss).
var CORNER = "sheet-corner";
var FILL = "sheet-fill";
// Size (px) of the fill handle's hit area in the corner cell.
var HANDLE = 9;
// Clipboard characters allowed per cell of the table on top of the cell cap:
// a tab or line break after the value, and a quote pair around it.
var PASTE_OVERHEAD = 3;
// Fastest a drag outside the grid auto-scrolls, in px per animation frame.
var MAX_STEP = 40;

// Excel/Sheets clipboard text to rows of fields: tab-separated, newline-ended,
// a field quoted only when it holds a tab, newline or quote ("" escapes one).
function parseTSV(text) {
  var rows = [];
  var row = [];
  var field = "";
  var quoted = false;
  var src = text.replace(/\r\n?/g, "\n").replace(/\n$/, "");
  for (var i = 0; i < src.length; i++) {
    var ch = src[i];
    if (quoted) {
      if (ch !== '"') field += ch;
      else if (src[i + 1] === '"') field += src[++i];
      else quoted = false;
    } else if (ch === '"' && field === "") {
      quoted = true;
    } else if (ch === "\t") {
      row.push(field);
      field = "";
    } else if (ch === "\n") {
      row.push(field);
      rows.push(row);
      row = [];
      field = "";
    } else {
      field += ch;
    }
  }
  row.push(field);
  rows.push(row);
  return rows;
}

// The inverse of parseTSV(), quoting exactly the fields a spreadsheet would.
function toTSV(rows) {
  return rows
    .map(function (row) {
      return row
        .map(function (v) {
          return /[\t\n\r"]/.test(v) ? '"' + v.replace(/"/g, '""') + '"' : v;
        })
        .join("\t");
    })
    .join("\n");
}

function clamp(v, lo, hi) {
  return Math.max(lo, Math.min(hi, v));
}

/**
 * Attaches the spreadsheet layer to a DataTables instance.
 *
 * @param {object} api DataTables API of the table (`this.api()` in initComplete).
 * @param {object} opts `inputId`: Shiny input receiving edit batches
 *   (`op: "edit"`) and undo/redo requests (`op: "undo"`/`"redo"`);
 *   `readonly`: data column indices never written; `lists`: data column index
 *   -> allowed values, edited through a dropdown; `maxLength`: characters a
 *   free-text editor accepts (the server's cap); `isActive`: returns whether
 *   the layer should currently handle input (false hands every event back).
 * @returns {{clear: function}} Handle to drop the current selection.
 */
export function attachSheet(api, opts) {
  var wrapper = api.table().container();
  var scroller = wrapper.querySelector(".dataTables_scroll") || wrapper;
  var lists = opts.lists || {};
  var readonly = {};
  (opts.readonly || []).forEach(function (i) {
    readonly[i] = true;
  });

  // ar/ac: anchor (active) cell, fr/fc: the corner the range extends to.
  var sel = null;
  // While dragging the fill handle: extend `down` (else right) up to `to`.
  var fill = null;
  var painted = [];
  // null, "select" or "fill".
  var drag = null;
  var moved = false;
  var editOnClick = false;
  var pointer = null;
  var raf = 0;

  wrapper.tabIndex = -1;
  wrapper.classList.add("sheet-grid");

  function focusGrid() {
    wrapper.focus({ preventScroll: true });
  }

  function send(msg) {
    Shiny.setInputValue(opts.inputId, msg, { priority: "event" });
  }

  function dims() {
    var rows = api.table().body().rows;
    var empty = !rows.length || rows[0].cells[0].classList.contains("dataTables_empty");
    return { rows: rows, nr: empty ? 0 : rows.length, nc: empty ? 0 : rows[0].cells.length };
  }

  // The body cell `el` sits in, or null for headers, filters and open editors.
  function bodyCell(el) {
    if (!el || !el.closest || el.closest("input, textarea, select")) return null;
    var td = el.closest("td");
    if (!td || td.parentNode.parentNode !== api.table().body()) return null;
    return td.classList.contains("dataTables_empty") ? null : td;
  }

  // An open DT editor `el` is (the input or dropdown in a body cell).
  function editorOf(el) {
    if (!el || !/^(INPUT|TEXTAREA|SELECT)$/.test(el.tagName)) return null;
    var td = el.closest("td");
    return td && td.parentNode.parentNode === api.table().body() ? el : null;
  }

  function range() {
    return {
      r0: Math.min(sel.ar, sel.fr),
      r1: Math.max(sel.ar, sel.fr),
      c0: Math.min(sel.ac, sel.fc),
      c1: Math.max(sel.ac, sel.fc),
    };
  }

  // The cells a fill drag would write, next to the range.
  function fillRange() {
    var g = range();
    if (!fill) return null;
    return fill.down
      ? { r0: g.r1 + 1, r1: fill.to, c0: g.c0, c1: g.c1 }
      : { r0: g.r0, r1: g.r1, c0: g.c1 + 1, c1: fill.to };
  }

  // Display rows -> DataTables row indices, visible positions -> column indices.
  function dataMap() {
    return {
      rows: api.rows({ search: "applied", order: "applied" }).indexes().toArray(),
      cols: api.columns(":visible").indexes().toArray(),
    };
  }

  function paint() {
    for (var i = 0; i < painted.length; i++) {
      painted[i].classList.remove(SEL, ACTIVE, CORNER, FILL);
    }
    painted = [];
    var d = dims();
    if (!sel || !d.nr) return;
    sel.ar = clamp(sel.ar, 0, d.nr - 1);
    sel.fr = clamp(sel.fr, 0, d.nr - 1);
    sel.ac = clamp(sel.ac, 0, d.nc - 1);
    sel.fc = clamp(sel.fc, 0, d.nc - 1);
    var mark = function (g, cls) {
      for (var r = g.r0; r <= g.r1; r++) {
        var cells = d.rows[r].cells;
        for (var c = g.c0; c <= g.c1; c++) {
          cells[c].classList.add(cls);
          painted.push(cells[c]);
        }
      }
    };
    var g = range();
    mark(g, SEL);
    if (fill) mark(fillRange(), FILL);
    d.rows[sel.ar].cells[sel.ac].classList.add(ACTIVE);
    d.rows[g.r1].cells[g.c1].classList.add(CORNER);
  }

  function select(r, c, extend) {
    if (extend && sel) {
      sel.fr = r;
      sel.fc = c;
    } else {
      sel = { ar: r, ac: c, fr: r, fc: c };
    }
    paint();
  }

  function clear() {
    sel = null;
    fill = null;
    paint();
  }

  // Scrolls `td` into view clear of the sticky header and the pinned column.
  function reveal(td) {
    var box = scroller.getBoundingClientRect();
    var cell = td.getBoundingClientRect();
    var head = scroller.querySelector(".dataTables_scrollHead");
    var pinned = td.parentNode.cells[0];
    var top = box.top + (head ? head.offsetHeight : 0);
    var bottom = box.top + scroller.clientTop + scroller.clientHeight;
    var left = box.left + (pinned !== td ? pinned.offsetWidth : 0);
    var right = box.left + scroller.clientLeft + scroller.clientWidth;
    if (cell.top < top) scroller.scrollTop -= top - cell.top;
    else if (cell.bottom > bottom) scroller.scrollTop += cell.bottom - bottom;
    if (pinned === td) return;
    if (cell.left < left) scroller.scrollLeft -= left - cell.left;
    else if (cell.right > right) scroller.scrollLeft += cell.right - right;
  }

  function move(dr, dc, extend, toEdge) {
    var d = dims();
    if (!sel || !d.nr) return;
    var r = extend ? sel.fr : sel.ar;
    var c = extend ? sel.fc : sel.ac;
    if (toEdge) {
      if (dr) r = dr < 0 ? 0 : d.nr - 1;
      if (dc) c = dc < 0 ? 0 : d.nc - 1;
    } else {
      r = clamp(r + dr, 0, d.nr - 1);
      c = clamp(c + dc, 0, d.nc - 1);
    }
    select(r, c, extend);
    reveal(d.rows[r].cells[c]);
  }

  // The range's raw values (not their rendered marks) as rows of text.
  function rangeValues(g) {
    var m = dataMap();
    var out = [];
    for (var r = g.r0; r <= g.r1; r++) {
      var data = api.row(m.rows[r]).data();
      var line = [];
      for (var c = g.c0; c <= g.c1; c++) {
        var v = data[m.cols[c]];
        line.push(v === null || v === undefined ? "" : String(v));
      }
      out.push(line);
    }
    return out;
  }

  // Sends one edit per writable cell of the h x w block at display (r0, c0).
  // `valueAt(i, j, current)` returns the cell's new text, or undefined to leave
  // it alone. Clipped to the table; returns the block's actual extent.
  function writeBlock(r0, c0, h, w, valueAt) {
    var d = dims();
    var m = dataMap();
    var r1 = Math.min(r0 + h, d.nr) - 1;
    var c1 = Math.min(c0 + w, d.nc) - 1;
    var batch = { op: "edit", isolate: [], col: [], value: [], skipped: 0 };
    for (var r = r0; r <= r1; r++) {
      var data = api.row(m.rows[r]).data();
      for (var c = c0; c <= c1; c++) {
        var col = m.cols[c];
        var v = valueAt(r - r0, c - c0, data[col]);
        if (v === undefined) continue;
        if (readonly[col]) {
          batch.skipped++;
          continue;
        }
        batch.isolate.push(String(data[0]));
        batch.col.push(col);
        batch.value.push(v);
      }
    }
    if (batch.isolate.length || batch.skipped) send(batch);
    return { r0: r0, c0: c0, r1: r1, c1: c1 };
  }

  function clearRange() {
    var g = range();
    writeBlock(g.r0, g.c0, g.r1 - g.r0 + 1, g.c1 - g.c0 + 1, function (i, j, cur) {
      return cur === null || cur === undefined || cur === "" ? undefined : "";
    });
  }

  // Pastes at the selection's top-left. A block that fits the selection a
  // whole number of times along an axis is tiled across it, so one copied
  // value fills the entire selection (the spreadsheet convention).
  function paste(text) {
    var block = parseTSV(text);
    var h = block.length;
    var w = block.reduce(function (n, row) {
      return Math.max(n, row.length);
    }, 0);
    var g = range();
    var sh = g.r1 - g.r0 + 1;
    var sw = g.c1 - g.c0 + 1;
    var out = writeBlock(
      g.r0,
      g.c0,
      sh > h && sh % h === 0 ? sh : h,
      sw > w && sw % w === 0 ? sw : w,
      function (i, j) {
        return block[i % h][j % w];
      },
    );
    sel = { ar: out.r0, ac: out.c0, fr: out.r1, fc: out.c1 };
    paint();
  }

  // Repeats the range's values over the cells the fill handle was dragged
  // across, then grows the selection to cover them.
  function applyFill() {
    var g = range();
    var f = fillRange();
    var src = rangeValues(g);
    var h = src.length;
    var w = src[0].length;
    var out = writeBlock(f.r0, f.c0, f.r1 - f.r0 + 1, f.c1 - f.c0 + 1, function (i, j) {
      return src[i % h][j % w];
    });
    sel = { ar: g.r0, ac: g.c0, fr: Math.max(g.r1, out.r1), fc: Math.max(g.c1, out.c1) };
  }

  // Opens DT's own editor on the active cell. A typed key replaces a text
  // value, or picks the first matching choice of a dropdown; date pickers
  // just open. Opened by mouse, a dropdown shows its list straight away.
  function openEditor(seed, byMouse) {
    var d = dims();
    if (!sel || !d.nr) return;
    if (readonly[api.columns(":visible").indexes()[sel.ac]]) return;
    var td = d.rows[sel.ar].cells[sel.ac];
    if (td.querySelector("input, textarea, select")) return;
    $(td).trigger("dblclick");
    var select = td.querySelector("select");
    if (select && byMouse) {
      try {
        select.showPicker();
      } catch (err) {
        // Unsupported browser: the closed dropdown is still usable.
      }
    }
    if (seed === undefined) return;
    var input = td.querySelector("input, textarea");
    if (select) {
      var hit = [].find.call(select.options, function (o) {
        return o.value.toLowerCase().indexOf(seed.toLowerCase()) === 0;
      });
      if (hit) select.value = hit.value;
    } else if (input && input.type !== "date") {
      input.value = seed;
    }
  }

  // Closes an editor without committing: DT's blur handler is what commits,
  // so it is unbound before the cell is re-rendered from its unchanged data.
  function cancelEditor(input) {
    var td = input.closest("td");
    $(input).off("blur");
    api.cell(td).invalidate();
    focusGrid();
  }

  function setFill(r, c) {
    var g = range();
    var next = null;
    if (r > g.r1 && r - g.r1 >= c - g.c1) next = { down: true, to: r };
    else if (c > g.c1) next = { down: false, to: c };
    if (JSON.stringify(next) === JSON.stringify(fill)) return;
    fill = next;
    paint();
  }

  // Follows a drag onto the cell under `el`: extends the range, or the fill.
  function track(el) {
    var td = bodyCell(el);
    if (!td || !sel) return;
    var r = td.parentNode.sectionRowIndex;
    var c = td.cellIndex;
    if (drag === "fill") {
      setFill(r, c);
      return;
    }
    if (r === sel.fr && c === sel.fc) return;
    moved = true;
    select(r, c, true);
  }

  // Keeps a drag that has left the grid's visible area scrolling toward the
  // pointer, extending to the edge cell nearest it.
  function autoScroll() {
    raf = 0;
    if (!drag || !pointer) return;
    var box = scroller.getBoundingClientRect();
    var head = scroller.querySelector(".dataTables_scrollHead");
    var top = box.top + (head ? head.offsetHeight : 0);
    var dy = pointer.y > box.bottom ? pointer.y - box.bottom : Math.min(pointer.y - top, 0);
    var dx = pointer.x > box.right ? pointer.x - box.right : Math.min(pointer.x - box.left, 0);
    if (!dx && !dy) return;
    scroller.scrollTop += clamp(dy, -MAX_STEP, MAX_STEP);
    scroller.scrollLeft += clamp(dx, -MAX_STEP, MAX_STEP);
    track(
      document.elementFromPoint(
        clamp(pointer.x, box.left + 1, box.right - MAX_STEP / 2),
        clamp(pointer.y, top + 1, box.bottom - MAX_STEP / 2),
      ),
    );
    raf = requestAnimationFrame(autoScroll);
  }

  // Mouse: press selects (Shift extends), drag spans a range, a click on the
  // cell that was already active opens its editor, and a press on the
  // corner's handle starts a fill.
  wrapper.addEventListener(
    "mousedown",
    function (e) {
      if (e.button !== 0 || !opts.isActive()) return;
      var td = bodyCell(e.target);
      if (!td) return;
      e.preventDefault();
      focusGrid();
      var box = td.getBoundingClientRect();
      if (
        td.classList.contains(CORNER) &&
        e.clientX >= box.right - HANDLE &&
        e.clientY >= box.bottom - HANDLE
      ) {
        drag = "fill";
        fill = null;
        return;
      }
      var r = td.parentNode.sectionRowIndex;
      var c = td.cellIndex;
      editOnClick =
        !e.shiftKey &&
        !!sel &&
        sel.ar === r &&
        sel.ac === c &&
        sel.fr === r &&
        sel.fc === c;
      select(r, c, e.shiftKey);
      drag = "select";
      moved = false;
    },
    true,
  );

  document.addEventListener("mousemove", function (e) {
    if (!drag) return;
    pointer = { x: e.clientX, y: e.clientY };
    track(e.target);
    if (!raf) raf = requestAnimationFrame(autoScroll);
  });

  document.addEventListener("mouseup", function () {
    if (!drag) return;
    var was = drag;
    drag = null;
    pointer = null;
    if (was === "fill") {
      if (fill) applyFill();
      fill = null;
      paint();
    } else if (editOnClick && !moved) {
      openEditor(undefined, true);
    }
  });

  // DT's own handlers must not see body clicks: here a click selects, and
  // editors open only through openEditor(). A native double-click lands on
  // the cell *after* the second click has already opened its editor, and
  // DT's dblclick handler would then open a second editor over the first,
  // saving the first one's markup as the cell's "original" content - which
  // is what it restores on blur, blanking the cell. openEditor() triggers
  // dblclick through jQuery, which never dispatches a native event, so this
  // capture listener does not stop it.
  ["click", "dblclick"].forEach(function (type) {
    wrapper.addEventListener(
      type,
      function (e) {
        if (opts.isActive() && bodyCell(e.target)) e.stopPropagation();
      },
      true,
    );
  });

  wrapper.addEventListener("keydown", function (e) {
    if (!opts.isActive()) return;
    var editor = editorOf(e.target);
    if (editor) {
      // Enter/Tab commit (DT commits on blur) and move on; Escape discards.
      if (e.key === "Enter" || e.key === "Tab") {
        e.preventDefault();
        focusGrid();
        if (e.key === "Enter") move(e.shiftKey ? -1 : 1, 0, false);
        else move(0, e.shiftKey ? -1 : 1, false);
      } else if (e.key === "Escape") {
        e.preventDefault();
        cancelEditor(editor);
      }
      return;
    }
    if (e.target !== wrapper) return;

    var mod = e.ctrlKey || e.metaKey;
    var key = e.key.toLowerCase();
    if (mod && (key === "z" || key === "y")) {
      e.preventDefault();
      send({ op: key === "y" || e.shiftKey ? "redo" : "undo" });
      return;
    }
    if (!sel) return;

    var arrows = { ArrowUp: [-1, 0], ArrowDown: [1, 0], ArrowLeft: [0, -1], ArrowRight: [0, 1] };
    if (arrows[e.key]) {
      move(arrows[e.key][0], arrows[e.key][1], e.shiftKey, mod);
    } else if (e.key === "Tab") {
      move(0, e.shiftKey ? -1 : 1, false);
    } else if (e.key === "Enter" || e.key === "F2") {
      openEditor();
    } else if (e.key === "Delete" || e.key === "Backspace") {
      clearRange();
    } else if (e.key === "Escape") {
      clear();
    } else if (mod && key === "a") {
      var d = dims();
      if (d.nr) sel = { ar: 0, ac: 0, fr: d.nr - 1, fc: d.nc - 1 };
      paint();
    } else if (e.key.length === 1 && !mod && !e.altKey) {
      openEditor(e.key);
    } else {
      return;
    }
    e.preventDefault();
  });

  // Clipboard events are listened for on the document and claimed by focus.
  // Outside an editable element the browser targets them at wherever the
  // page's text selection last was - not at the focused grid - and since a
  // press on the grid never moves that selection, their target is unrelated
  // to the grid and unreliable to check.
  function gridFocused() {
    return opts.isActive() && !!sel && document.activeElement === wrapper;
  }

  document.addEventListener("copy", function (e) {
    if (!gridFocused()) return;
    e.clipboardData.setData("text/plain", toTSV(rangeValues(range())));
    e.preventDefault();
  });

  document.addEventListener("cut", function (e) {
    if (!gridFocused()) return;
    e.clipboardData.setData("text/plain", toTSV(rangeValues(range())));
    e.preventDefault();
    clearRange();
  });

  // The most clipboard text a paste into this table can sensibly hold: every
  // displayed cell filled to the cap, plus separators and quotes. Anything
  // longer cannot be a block of valid values for this table, so it is refused
  // before parsing - which is also what keeps an accidental paste of a huge
  // document from freezing the tab while it is split into cells.
  function pasteLimit() {
    var d = dims();
    return d.nr * d.nc * ((opts.maxLength || 0) + PASTE_OVERHEAD);
  }

  // Pasting into the grid, or a multi-cell block into an open editor (which
  // would otherwise squash it into one cell), pastes it across the cells. A
  // single value pasted into an open editor stays a native paste.
  document.addEventListener("paste", function (e) {
    if (!opts.isActive() || !sel) return;
    var editor = editorOf(document.activeElement);
    if (!gridFocused() && !editor) return;
    var text = e.clipboardData.getData("text/plain");
    if (opts.maxLength && text.length > pasteLimit()) {
      e.preventDefault();
      if (window.Shiny && Shiny.notifications) {
        Shiny.notifications.show({
          html: "The clipboard holds more text than this table can take.",
          type: "warning",
          duration: 6000,
        });
      }
      return;
    }
    var block = /[\t\n]/.test(text.replace(/\r?\n$/, ""));
    if (!gridFocused() && !block) return;
    e.preventDefault();
    if (editor) cancelEditor(editor);
    if (text) paste(text);
  });

  // A cell drawn from a fixed list (category / yes-no) gets a native <select>
  // swapped in for DT's text input the moment it is created, offering exactly
  // those choices. The input stays in the DOM, hidden, so DT's own blur
  // handler and the Shiny wiring behind it still do the commit: this only
  // writes the chosen option back into it and triggers blur. Committing on
  // blur is synchronous on purpose - a click on another cell blurs this
  // select first, and a deferred commit would land its redraw on top of the
  // next editor.
  $(api.table().node()).on("focusin", "input", function () {
    var $input = $(this);
    var td = this.closest("td");
    var idx = td && !$input.data("listSwapped") ? api.cell(td).index() : null;
    var options = idx ? lists[idx.column] : null;
    if (!options || !options.length) return;

    $input.data("listSwapped", true);
    var current = $input.val();
    var $select = $('<select class="dt-category-select"></select>');
    options.forEach(function (v) {
      $select.append($("<option></option>").val(v).text(v));
    });
    // A stored value outside the list (a level since removed) stays visible.
    if (options.indexOf(current) === -1) {
      $select.prepend($("<option></option>").val(current).text(current));
    }
    $select.val(current);

    var commit = function () {
      var chosen = $select.val();
      $select.off().remove();
      $input.val(chosen);
      $input.trigger("blur");
    };
    // A choice picked from the open list commits at once. One stepped to
    // with the keyboard (arrows, type-ahead) only changes the selection -
    // Enter/Tab commit it, as in any other editor.
    var keyed = false;
    $select.on("keydown", function () {
      keyed = true;
    });
    $select.on("change", function () {
      if (keyed) return;
      commit();
      focusGrid();
    });
    $select.on("blur", commit);

    $input.hide().after($select);
    $select.trigger("focus");
  });

  // Typing stops at the server's cap. A pasted block is checked on the server
  // instead, so an over-long value there is rejected, never silently cut.
  if (opts.maxLength) {
    $(api.table().node()).on("focusin", "input[type=text], textarea", function () {
      this.maxLength = opts.maxLength;
    });
  }

  api.on("draw.dt", paint);
  api.on("column-visibility.dt", clear);

  return { clear: clear };
}
