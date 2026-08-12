/**
 * Keeps DataTables header and body columns aligned when horizontal scrolling (`scrollX`) is enabled.
 *
 * DataTables renders separate <table> elements for header and body. Initial dimensions 
 * can misalign before flex layouts settle or vertical scrollbars appear. This script 
 * tracks scrollport width changes via ResizeObserver and triggers `columns.adjust()` 
 * to restore alignment.
 *
 * Scoped to layout-overridden table classes defined by SELECTOR.
 */

var SELECTOR = ".edit-table, .isolate-selection-table";
var SCROLLPORT = ".dataTables_scroll";
var DEBOUNCE_MS = 100;

// Tracks observed DOM elements and their last measured widths (keyed by element reference).
var tracked = new WeakMap();

// Lock flag to prevent infinite loops from ResizeObserver callbacks triggered by `columns.adjust()`.
var settling = false;

function adjustColumns(wrapper) {
  var table = wrapper.querySelector("table.dataTable");
  if (!table || !window.$ || !$.fn.dataTable) return;
  if (!$.fn.dataTable.isDataTable(table)) return;

  settling = true;
  try {
    $(table).DataTable().columns.adjust();
  } finally {
    // Wait two animation frames for layout reflow and observer events to settle before clearing the guard.
    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        settling = false;
      });
    });
  }
}

var observer = new ResizeObserver(function (entries) {
  entries.forEach(function (entry) {
    var width = Math.round(entry.contentRect.width);

    // Ignore hidden elements (e.g., inside inactive tabs or closed modals).
    if (!width) return;

    var state = tracked.get(entry.target);
    if (!state) return;

    // Skip unchanged dimensions.
    if (state.width === width) return;
    state.width = width;

    // Ignore width changes caused directly by our own adjust calls.
    if (settling) return;

    var wrapper = state.wrapper;
    var wrapperState = tracked.get(wrapper);
    clearTimeout(wrapperState.timer);
    wrapperState.timer = setTimeout(function () {
      adjustColumns(wrapper);
    }, DEBOUNCE_MS);
  });
});

function observeElement(element, wrapper) {
  if (!element || tracked.has(element)) return;
  tracked.set(element, { wrapper: wrapper });
  observer.observe(element);
}

function scan() {
  document.querySelectorAll(SELECTOR).forEach(function (root) {
    root.querySelectorAll(".dataTables_wrapper").forEach(function (wrapper) {
      observeElement(wrapper, wrapper);
      // Re-query scrollport since DataTables recreates it on dynamic re-renders.
      observeElement(wrapper.querySelector(SCROLLPORT), wrapper);
    });
  });
}

// Rescan DOM when Shiny updates outputs or Bootstrap modals reveal hidden tables.
$(document).on("shiny:value", function () {
  setTimeout(scan, 0);
});
$(document).on("shown.bs.modal", function () {
  setTimeout(scan, 0);
});
document.addEventListener("DOMContentLoaded", scan);
scan();