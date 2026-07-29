// Apply a whole column-visibility change to a DataTable in one measuring pass.
//
// DT's proxy API only offers showCols() and hideCols(), and each one ends in an
// unconditional columns.adjust() (see `columns().visible()` in DataTables:
// `calc === undefined` still adjusts). A picker session that both reveals and
// hides columns therefore has to call both, and pays for two full re-measures —
// and dt-column-sizing.js then adds a third when the resulting width change
// reaches its ResizeObserver.
//
// That measuring pass is the expensive half by far. It walks every column
// forcing a layout against a table that, with `paging = FALSE`, has every row
// in the DOM: on the Browse Entries table (~85 columns x a few hundred
// isolates) one pass runs into seconds, and toggling a whole AMR category used
// to block the main thread long enough for Chrome to offer "Page Unresponsive".
//
// DataTables already supports batching this — `visible(state, false)` performs
// the DOM surgery and skips the recalculation — there is just no way to reach
// the second argument through the proxy. So drive it directly: both sets of
// columns change, then one adjust for the pair.
//
// The lookup is also allowed to miss. `input$col_picker` can fire after the
// output that owned the table is gone (a session reset tears the panel down
// while the picker's last value is still in flight), and DT's own proxy path
// logs "Couldn't find table with id ..." when that happens. Here it is simply
// a no-op.
Shiny.addCustomMessageHandler("pt-dt-column-visibility", function (message) {
  if (!window.$ || !window.Shiny) return;

  // The output id belongs to the htmlwidget's container <div>, not to the
  // <table> DataTables was initialised on - so this cannot go through
  // $().DataTable() on the element itself. DT hangs the API off the container
  // under `data('datatable')`, which is the same handle its own proxy methods
  // resolve (see the 'datatable-calls' handler in DT's datatables.js).
  var el = document.getElementById(message.id);
  var table = el ? $(el).data("datatable") : null;
  if (!table) return;

  // Shiny unwraps a length-1 vector to a scalar; DataTables' column selector
  // takes either, but the `.length` guards below need the array form.
  var hide = [].concat(message.hide === undefined ? [] : message.hide);
  var show = [].concat(message.show === undefined ? [] : message.show);
  if (!hide.length && !show.length) return;

  // `false` as the second argument suppresses DataTables' own trailing
  // re-measure on each call. Nothing is lost by it: changing visibility still
  // announces column-visibility.dt per column, and the table's initComplete
  // (database_browser.R) coalesces that burst into exactly one
  // columns.adjust().draw(false) once both sets are in their final state.
  // Measuring here as well would only repeat that pass.
  if (hide.length) table.columns(hide).visible(false, false);
  if (show.length) table.columns(show).visible(true, false);
});
