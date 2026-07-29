// Keep a DataTable's header and body columns sized from the same measurement.
//
// With scrollX, DT renders the header and the body as two separate <table>s
// and sizes them by measuring once and copying the result onto the header.
// That measurement is taken as the widget is inserted, before the surrounding
// flex layout has settled: the scrollport does not yet have its final height,
// so it has no vertical scrollbar, and DT sizes the columns for a scrollport a
// scrollbar's width wider than the one they end up in. The header keeps those
// too-wide columns while the body reflows into the real width, and the two
// stop lining up.
//
// DT recomputes correctly whenever it is asked to, so ask it whenever the
// width it measures against actually changes — that is the scrollport's inner
// width, which moves both when the pane is resized (window, sidebar, modal,
// tab switch) and when the vertical scrollbar appears or disappears. Reacting
// to the measurement rather than guessing a settling delay is what makes this
// converge: DT recomputes to a fixed point, so once the width stops changing
// the guard below stops any further work.
//
// Scoped to the two table families whose scroll layout the app overrides — see
// "One scrollport per table" in main.scss. Every other DataTable in the app
// keeps stock DT behaviour.
var SELECTOR = ".edit-table, .isolate-selection-table";
var SCROLLPORT = ".dataTables_scroll";
var DEBOUNCE_MS = 100;

// Elements already under observation, and the width each was last seen at.
// Keyed on the element so a re-rendered table (Shiny replaces the whole
// wrapper) is picked up again by the next scan rather than being mistaken for
// one we have already handled.
var tracked = new WeakMap();

// True while an adjust of our own is settling. columns.adjust() writes column
// widths, which moves the scrollport's own width, which comes straight back
// through the observer below — the callback that would schedule the next
// adjust. Reacting to that is at best redundant work and at worst endless:
// convergence is DT's behaviour, not a guarantee, and every extra pass on a
// wide unpaged table costs seconds of blocked main thread. Only widths that
// something *else* changed are worth reacting to.
var settling = false;

function adjustColumns(wrapper) {
  var table = wrapper.querySelector("table.dataTable");
  if (!table || !window.$ || !$.fn.dataTable) return;
  if (!$.fn.dataTable.isDataTable(table)) return;

  settling = true;
  try {
    $(table).DataTable().columns.adjust();
  } finally {
    // Two frames: one for the layout the adjust dirtied, one for the observer
    // callback it delivers. Whatever widths have settled by then become the
    // new baseline, so the next real change is still measured against them.
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
    // Zero while the table sits in an inactive tab or an unopened modal; the
    // real measurement arrives when it is shown.
    if (!width) return;

    var state = tracked.get(entry.target);
    if (!state) return;
    // The first callback fires on observe() with no recorded width, which is
    // what corrects the initial draw.
    if (state.width === width) return;
    state.width = width;

    // Recorded above rather than below the guard: the width an adjust of ours
    // produced is the baseline the next genuine resize has to differ from.
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
      // The scrollport only exists once DT has drawn, and is replaced on
      // re-render, so it is looked up on every scan rather than just the first.
      observeElement(wrapper.querySelector(SCROLLPORT), wrapper);
    });
  });
}

// Tables arrive (and are replaced) as Shiny renders outputs, and modal-hosted
// ones only get a size once the modal is shown.
$(document).on("shiny:value", function () {
  setTimeout(scan, 0);
});
$(document).on("shown.bs.modal", function () {
  setTimeout(scan, 0);
});
document.addEventListener("DOMContentLoaded", scan);
scan();
