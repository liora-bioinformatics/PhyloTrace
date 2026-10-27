/**
 * Applies cell values the server pushes into a client-side editable grid
 * ("pt-dt-patch", sent by app/logic/sheet_grid.R), in place of DT's
 * replaceData() - which only exists for server-side tables, and reloads and
 * redraws every row.
 *
 * A patch names cells by DataTables row and column index and carries each
 * one's value as the server now stores it: an edit's accepted or normalised
 * value, a rejected edit's previous one, a pasted block, an undo. Only cells
 * whose value actually differs from what the grid holds cost a redraw, so the
 * reply to an ordinary typed edit (the grid already shows it) draws nothing.
 * A patch carrying `data` (columns of values) replaces every row instead.
 *
 * An open editor is never torn out by a patch. A cell being edited takes the
 * value into its data only and is re-rendered once the editor closes, and the
 * redraw a changed value needs waits for the same moment.
 */

// Per table node: `cells` re-rendered and `draw` (a redraw owed) once the open
// editor closes.
var held = new WeakMap();

function text(v) {
  return v === null || v === undefined ? "" : String(v);
}

function editorIn(node) {
  return node ? node.querySelector("input, textarea, select") : null;
}

// Settles what a patch held back, once no editor is open any more.
function release(api) {
  var table = api.table().node();
  var state = held.get(table);
  if (!state) return;
  if (editorIn(api.table().body())) {
    awaitClose(api);
    return;
  }
  held.delete(table);
  state.cells.forEach(function (cell) {
    api.cell(cell[0], cell[1]).invalidate();
  });
  if (state.draw) api.draw(false);
}

// DT commits or restores an editor on blur, before focusout reaches here.
function awaitClose(api) {
  $(api.table().body()).one("focusout", function () {
    setTimeout(function () {
      release(api);
    }, 0);
  });
}

function hold(api) {
  var table = api.table().node();
  var state = held.get(table);
  if (state) return state;
  state = { cells: [], draw: false };
  held.set(table, state);
  awaitClose(api);
  return state;
}

// Replaces every row with `columns` (one array of values per column).
function replaceAll(api, columns) {
  var rows = [];
  var n = columns.length ? columns[0].length : 0;
  for (var i = 0; i < n; i++) {
    rows.push(
      columns.map(function (col) {
        return col[i];
      }),
    );
  }
  api.clear();
  api.rows.add(rows);
  api.draw(false);
}

Shiny.addCustomMessageHandler("pt-dt-patch", function (msg) {
  var el = document.getElementById(msg.id);
  var api = el ? $(el).data("datatable") : null;
  if (!api) return;
  if (msg.data) {
    replaceAll(api, msg.data);
    return;
  }
  var settings = api.settings()[0];
  var rows = [].concat(msg.rows);
  var cols = [].concat(msg.cols);
  var values = [].concat(msg.values);
  var redraw = false;
  for (var k = 0; k < rows.length; k++) {
    var row = settings.aoData[rows[k]];
    if (!row) continue;
    var col = cols[k];
    if (text(row._aData[col]) !== text(values[k])) redraw = true;
    var td = row.anCells ? row.anCells[col] : null;
    if (editorIn(td)) {
      // DataTables' own cell().data() would re-render the cell, and the
      // editor with it: write the value and drop the caches it sorts and
      // filters by, as cell().data() does, and render once the editor closes.
      row._aData[col] = values[k];
      row._aSortData = null;
      row._aFilterData = null;
      settings.aoColumns[col].sType = null;
      hold(api).cells.push([rows[k], col]);
    } else {
      api.cell(rows[k], col).data(values[k]);
    }
  }
  if (!redraw) return;
  if (editorIn(api.table().body())) hold(api).draw = true;
  else api.draw(false);
});
