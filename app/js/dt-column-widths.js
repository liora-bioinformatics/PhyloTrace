/**
 * Pins the column widths of a windowed grid (app/js/dt-grid-window.js) so they
 * hold while the drawn window moves.
 *
 * Left to itself the browser lays a table out from the rows it holds, and a
 * windowed grid holds only the rows in view: each move of the window would
 * re-fit every column to that window's values. Instead each column is sized
 * once from its widest value across all rows, drawn or not, and from its
 * header, and the widths are written as a <colgroup> into the header and body
 * tables, which lay out with `table-layout: fixed` (main.scss,
 * `.dt-pinned-cols`). The two tables then share one set of widths, and drawn
 * content can no longer resize a column.
 *
 * A column only ever grows after that: a draw that brings in a value wider
 * than its column (an edit, a pushed value) widens it to fit.
 */

// Candidates per column, by text length, that are measured in pixels: the
// longest string is not always the widest one.
var CANDIDATES = 8;
// Covers fractional text widths and rounding between the two tables.
var SLACK = 2;

var canvas = document.createElement("canvas").getContext("2d");
var decoder = document.createElement("div");

// Text of a cell's display value: rendered HTML without its tags, entities
// decoded (the grid holds its text HTML-escaped).
function plainText(html) {
  if (html === null || html === undefined) return "";
  var s = String(html);
  if (!/[<&]/.test(s)) return s;
  decoder.innerHTML = s;
  return decoder.textContent;
}

// Width of `text` laid out in the font of computed style `cs`.
function textWidth(text, cs) {
  canvas.font = cs.font;
  if ("letterSpacing" in canvas) canvas.letterSpacing = cs.letterSpacing;
  if (cs.textTransform === "uppercase") text = text.toUpperCase();
  return canvas.measureText(text).width;
}

// Horizontal padding and border of a cell, plus its CSS width floor.
function box(cs) {
  var extra = 0;
  ["paddingLeft", "paddingRight", "borderLeftWidth", "borderRightWidth"].forEach(function (k) {
    extra += parseFloat(cs[k]) || 0;
  });
  return { extra: extra, floor: parseFloat(cs.minWidth) || 0 };
}

// Raw values of column `col` for the rows at `rows` (indexes into aoData),
// reduced to the CANDIDATES longest ones.
function longest(settings, rows, col) {
  var best = [];
  for (var i = 0; i < rows.length; i++) {
    var raw = settings.aoData[rows[i]]._aData[col];
    var len = raw === null || raw === undefined ? 0 : String(raw).length;
    if (best.length === CANDIDATES && len <= best[CANDIDATES - 1].len) continue;
    best.push({ row: rows[i], len: len });
    best.sort(function (a, b) {
      return b.len - a.len;
    });
    if (best.length > CANDIDATES) best.pop();
  }
  return best;
}

/**
 * Attaches pinned column widths to a windowed grid.
 *
 * @param {object} api DataTables API of the table.
 * @returns {object} `refit()`, to size every column afresh from all rows.
 */
export function attachColumnWidths(api) {
  var settings = api.settings()[0];
  var getCell = $.fn.dataTable.ext.internal._fnGetCellData;
  var wrapper = api.table().container();
  // Pixel width per column index, for the visible columns.
  var widths = {};
  // Body cell style per column index, read from a drawn cell.
  var styles = {};

  function tables() {
    return wrapper.querySelectorAll(".dataTables_scroll table");
  }

  // Style of the body cells of `col`: from a drawn cell, as the font (italic
  // read-only columns) and floor (date columns) differ per column.
  function cellStyle(col) {
    var row = api.table().body().rows[0];
    var vis = api.column(col).index("visible");
    var cell = row && !row.querySelector(".dataTables_empty") ? row.cells[vis] : null;
    if (cell) styles[col] = getComputedStyle(cell);
    return styles[col];
  }

  // Width the widest of `rows`' values in `col` needs.
  function bodyWidth(col, rows) {
    var cs = cellStyle(col);
    if (!cs) return 0;
    var b = box(cs);
    var max = 0;
    longest(settings, rows, col).forEach(function (c) {
      var text = plainText(getCell(settings, c.row, col, "display"));
      max = Math.max(max, textWidth(text, cs));
    });
    return Math.max(b.floor, max + b.extra);
  }

  // Width a header cell's label needs, as laid out (text-transform, letter
  // spacing and the sort icon's padding included).
  function headerWidth(th) {
    var range = document.createRange();
    range.selectNodeContents(th);
    var cs = getComputedStyle(th);
    return range.getBoundingClientRect().width + box(cs).extra;
  }

  // Header cells by the visible columns they span, from the header table's
  // own grid (rowspans and colspans) rather than DataTables' model of it.
  function headerSpans() {
    var head = wrapper.querySelector(".dataTables_scrollHead thead");
    if (!head) return [];
    var taken = [];
    var spans = [];
    Array.prototype.forEach.call(head.rows, function (tr, r) {
      taken[r] = taken[r] || [];
      var c = 0;
      Array.prototype.forEach.call(tr.cells, function (cell) {
        while (taken[r][c]) c++;
        for (var dr = 0; dr < cell.rowSpan; dr++) {
          taken[r + dr] = taken[r + dr] || [];
          for (var dc = 0; dc < cell.colSpan; dc++) taken[r + dr][c + dc] = true;
        }
        // Filter row cells hold inputs that fill whatever width they get.
        if (cell.tagName === "TH") spans.push({ cell: cell, start: c, span: cell.colSpan });
        c += cell.colSpan;
      });
    });
    return spans;
  }

  // Writes the widths into a <colgroup> heading each table. DataTables
  // re-clones the body's header on every draw, so the colgroup is put back in
  // front of it each time.
  function apply() {
    var cols = api.columns(":visible").indexes().toArray();
    var total = 0;
    var html = cols
      .map(function (col) {
        total += widths[col];
        return '<col style="width:' + widths[col] + 'px">';
      })
      .join("");
    Array.prototype.forEach.call(tables(), function (table) {
      var group = table.querySelector(":scope > colgroup.dt-pinned");
      if (!group) {
        group = document.createElement("colgroup");
        group.className = "dt-pinned";
      }
      if (group.innerHTML !== html) group.innerHTML = html;
      if (table.firstChild !== group) table.insertBefore(group, table.firstChild);
      table.classList.add("dt-pinned-cols");
    });
    wrapper.style.setProperty("--dt-pinned-width", total + "px");
  }

  // Sizes every visible column from all rows and its header label.
  function refit() {
    var cols = api.columns(":visible").indexes().toArray();
    if (!cols.length || !wrapper.offsetWidth) return;
    var all = [];
    for (var i = 0; i < settings.aoData.length; i++) all.push(i);
    widths = {};
    cols.forEach(function (col) {
      widths[col] = bodyWidth(col, all);
    });
    // Single-column header cells first, then any label spanning several
    // columns widens the last of them by what it lacks.
    var spans = headerSpans().sort(function (a, b) {
      return a.span - b.span;
    });
    spans.forEach(function (h) {
      var covered = cols.slice(h.start, h.start + h.span);
      if (!covered.length) return;
      var have = covered.reduce(function (sum, col) {
        return sum + widths[col];
      }, 0);
      var need = headerWidth(h.cell);
      if (need > have) widths[covered[covered.length - 1]] += need - have;
    });
    cols.forEach(function (col) {
      widths[col] = Math.ceil(widths[col] + SLACK);
    });
    apply();
  }

  // Widens any column the drawn rows have outgrown; never narrows one. Runs
  // on every draw, which is also what puts the body's colgroup back.
  function grow() {
    var cols = api.columns(":visible").indexes().toArray();
    if (cols.some(function (col) { return !(col in widths); })) {
      refit();
      return;
    }
    var drawn = settings.aiDisplay.slice(settings._iDisplayStart, settings.fnDisplayEnd());
    cols.forEach(function (col) {
      widths[col] = Math.max(widths[col], Math.ceil(bodyWidth(col, drawn) + SLACK));
    });
    apply();
  }

  api.on("draw.dt", grow);
  api.on("column-visibility.dt", function () {
    // After DataTables has rebuilt the header for the new set of columns.
    setTimeout(refit, 0);
  });
  // Text measured before the web font loaded is measured in its fallback.
  if (document.fonts && document.fonts.ready) document.fonts.ready.then(refit);
  refit();

  return { refit: refit };
}
