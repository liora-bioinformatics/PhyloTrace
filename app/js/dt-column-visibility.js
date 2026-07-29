/**
 * Custom Shiny message handler for batch DataTables column visibility updates.
 *
 * Bypasses standard DT proxy methods (`showCols`/`hideCols`) to avoid redundant, 
 * thread-blocking layout recalculations (`columns.adjust()`). Passing `false` as the 
 * second parameter to `visible()` performs DOM updates without triggering an 
 * immediate table re-measurement.
 */
Shiny.addCustomMessageHandler("pt-dt-column-visibility", function (message) {
  if (!window.$ || !window.Shiny) return;

  // Resolve DT instance attached to the htmlwidget container element.
  var el = document.getElementById(message.id);
  var table = el ? $(el).data("datatable") : null;
  if (!table) return;

  // Normalize scalar or undefined payloads to arrays.
  var hide = [].concat(message.hide === undefined ? [] : message.hide);
  var show = [].concat(message.show === undefined ? [] : message.show);
  if (!hide.length && !show.length) return;

  // Update visibility in DOM while suppressing trailing auto-recalculations (2nd arg = false).
  if (hide.length) table.columns(hide).visible(false, false);
  if (show.length) table.columns(show).visible(true, false);
});