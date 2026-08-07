/**
 * Redraws DataTables instances when their container transitions from hidden
 * to visible (a Bootstrap tab pane taking `shown.bs.tab`, or a bslib
 * accordion panel taking `shown.bs.collapse`).
 *
 * A table whose renderDT() output or dataTableProxy()/replaceData() update
 * lands while its pane is `display:none` (inactive tab) or mid-collapse
 * (accordion panel not yet open) can end up out of sync with the data it was
 * actually given - the DOM update happens, but the widget never repaints
 * against it. columns.adjust() alone (see dt-column-sizing.js) only fixes
 * column widths, not a stale draw, so this forces a real redraw of whatever
 * data the table currently holds. Unscoped by design: unlike the
 * scrollX-specific fix in dt-column-sizing.js, any DataTable on the page can
 * go stale this way, not just the ones with fixed columns.
 */

function resyncTables(root) {
  if (!root || !window.$ || !$.fn.dataTable) return;
  root.querySelectorAll("table.dataTable").forEach(function (table) {
    if (!$.fn.dataTable.isDataTable(table)) return;
    $(table).DataTable().columns.adjust().draw(false);
  });
}

$(document).on("shown.bs.tab", function (event) {
  var pane = event.target.hash ? document.querySelector(event.target.hash) : null;
  resyncTables(pane);
});

$(document).on("shown.bs.collapse", function (event) {
  resyncTables(event.target);
});
