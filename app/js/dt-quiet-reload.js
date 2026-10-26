/**
 * Lets the server reload a DataTable without its processing indicator.
 *
 * The editable grids (Browse Entries, Custom Variables) push every accepted
 * cell edit back with replaceData(), a server-side reload. Their processing
 * indicator is an overlay over the whole grid (main.scss) - right for a sort
 * or a filter, but in the way of typing value after value: it would come up
 * after each edit and catch the click meant for the next cell. The server
 * sends "pt-dt-quiet-reload" right before such a reload, and the indicator
 * stays hidden for that one request.
 *
 * A request is recognised by DataTables' draw counter, which it advances as
 * it shows the indicator, so a sort straight after an edit still shows it,
 * and edits in quick succession (whose superseded responses DataTables drops
 * without drawing) never leave it stuck hidden.
 */

// Per table node: `pending` (the next request is quiet) and `draw` (the
// counter of the quiet request in flight).
var quiet = new WeakMap();

Shiny.addCustomMessageHandler("pt-dt-quiet-reload", function (message) {
  var el = document.getElementById(message.id);
  var api = el ? $(el).data("datatable") : null;
  if (!api) return;
  var table = api.table().node();
  var state = quiet.get(table) || {};
  state.pending = true;
  quiet.set(table, state);
});

// DataTables shows the indicator, then fires `processing`, in one go - so the
// class is in place before the indicator can be painted.
$(document).on("processing.dt", function (e, settings, processing) {
  var table = settings.nTable;
  var state = quiet.get(table);
  if (!state) return;
  if (processing && state.pending) {
    state.pending = false;
    state.draw = settings.iDraw;
  }
  var wrapper = table.closest(".dataTables_wrapper");
  if (wrapper) {
    wrapper.classList.toggle("dt-quiet", processing && state.draw === settings.iDraw);
  }
});
