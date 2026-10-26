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
 *
 * Only a table that may be stale is redrawn. A redraw of a server-side table
 * refetches and rebuilds every row - seconds on a large Browse Entries grid -
 * so a table last drawn while visible, with no render waiting and nothing
 * drawn since, keeps its rows and only has its columns re-measured if its
 * width changed while it was hidden.
 */

// Per table node: `stale` (drawn while hidden, or never seen drawn while
// visible) and the wrapper `width` its columns were last sized for.
var seen = new WeakMap();

// Hidden: no box at all (a display:none ancestor), or inside an accordion
// panel that is closed or still opening.
function isHidden(table) {
  if (!table.offsetWidth || !table.offsetHeight) return true;
  return !!table.closest(".collapsing, .collapse:not(.show)");
}

function wrapperWidth(table) {
  var wrapper = table.closest(".dataTables_wrapper");
  return wrapper ? wrapper.clientWidth : 0;
}

// Records every draw and column sizing: whether it happened out of sight, and
// the width it was done at.
$(document).on("draw.dt column-sizing.dt", function (e) {
  var table = e.target;
  if (!table || table.tagName !== "TABLE") return;
  if (isHidden(table)) {
    seen.set(table, { stale: true, width: 0 });
  } else if (e.type === "draw" || !(seen.get(table) || {}).stale) {
    seen.set(table, { stale: false, width: wrapperWidth(table) });
  }
});

// A widget that received a render while hidden holds it back as `data` (DT's
// lazy render) until it is shown.
function renderPending(table) {
  var el = table.closest(".datatables.html-widget");
  if (!el || !window.HTMLWidgets) return false;
  var instance = HTMLWidgets.getInstance(el);
  return !!(instance && instance.data);
}

function resyncTables(root) {
  if (!root || !window.$ || !$.fn.dataTable) return;
  root.querySelectorAll("table.dataTable").forEach(function (table) {
    if (!$.fn.dataTable.isDataTable(table)) return;
    // Still out of sight (an inactive inner tab, a closed accordion panel):
    // a draw now would itself go stale. Its own `shown` event resyncs it.
    if (isHidden(table)) return;
    var state = seen.get(table);
    var api = $(table).DataTable();
    if (!state || state.stale || renderPending(table)) {
      api.columns.adjust().draw(false);
    } else if (state.width !== wrapperWidth(table)) {
      api.columns.adjust();
    }
  });
}

$(document).on("shown.bs.tab", function (event) {
  var pane = event.target.hash ? document.querySelector(event.target.hash) : null;
  resyncTables(pane);
});

$(document).on("shown.bs.collapse", function (event) {
  resyncTables(event.target);
});
