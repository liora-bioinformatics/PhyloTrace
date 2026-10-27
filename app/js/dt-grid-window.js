/**
 * Draws only the rows of an editable grid that are in view, inside its one
 * scrollport (main.scss, "One scrollport per table").
 *
 * The grid pages through its rows (`paging`, `deferRender`) without showing
 * it: one page is a window of rows somewhat taller than the view, and this
 * moves the window as the grid scrolls. The rows above and below the window
 * are stood in for by padding on the scroll body, so the scrollbar spans the
 * whole table. DataTables' Scroller extension does the same, but needs the
 * body to be a scroll container of its own - which is the two-scrollport
 * layout these grids moved away from.
 *
 * A draw then costs a window's worth of rows rather than every row, which is
 * what an edit, a sort or a filter used to wait on. Rows keep their display
 * positions in DataTables' own order (`api.page.info().start` is the first
 * drawn one), so the spreadsheet layer (app/js/dt-sheet.js) and row drag
 * (app/js/dt-row-drag.js) address rows outside the window the same way.
 */

// The window covers up to this many view heights...
var SCREENS = 3;
// ...as long as it stays within this many cells: a grid showing hundreds of
// AMR gene columns draws fewer rows per window, down to two view heights.
var MAX_CELLS = 8000;
// From this many shown columns on, re-measuring the header waits for a pause
// in drawing (see attachGridWindow()).
var WIDE_COLS = 60;
var SETTLE_MS = 200;

// An open DT cell editor in the grid.
function editing(api) {
  return !!api.table().body().querySelector("input, textarea, select");
}

/**
 * Attaches windowed drawing to a grid that pages without page controls.
 *
 * @param {object} api DataTables API of the table (`this.api()` in initComplete).
 */
export function attachGridWindow(api) {
  var settings = api.settings()[0];
  var wrapper = api.table().container();
  var scroll = wrapper.querySelector(".dataTables_scroll");
  var body = wrapper.querySelector(".dataTables_scrollBody");
  if (!scroll || !body || !settings.oFeatures.bPaginate) return;
  // Height of a drawn row, the unit the padding stands in for; 0 until
  // measured, and again once the columns or the view change.
  var rowHeight = 0;
  // Whether the window has been sized for the current columns.
  var sized = false;
  var queued = false;
  var resync = 0;

  // Rows all draw at one height (cells never wrap), so the average of a drawn
  // window holds for the rows outside it. Not taken while an editor is open:
  // its input can make that one row taller. Taken only when needed, as it
  // makes the browser lay the drawn rows out there and then.
  function measure() {
    var rows = api.table().body().rows;
    if (!rows.length || rows[0].querySelector(".dataTables_empty") || editing(api)) return;
    var height = api.table().body().offsetHeight / rows.length;
    if (height > 0) rowHeight = height;
  }

  function pad() {
    var start = settings._iDisplayStart;
    var drawn = settings.fnDisplayEnd() - start;
    var below = Math.max(0, settings.fnRecordsDisplay() - start - drawn);
    body.style.paddingTop = rowHeight * start + "px";
    body.style.paddingBottom = rowHeight * below + "px";
  }

  // Moves the window over the rows in view when they come near its edge.
  // Held while an editor is open: a redraw detaches the rows, and with them
  // the editor the user is typing into.
  function update() {
    queued = false;
    if (!scroll.clientHeight || editing(api)) return;
    if (!rowHeight) {
      measure();
      if (!rowHeight) return;
      pad();
    }
    var head = scroll.querySelector(".dataTables_scrollHead");
    var view = Math.max(0, scroll.clientHeight - (head ? head.offsetHeight : 0));
    var count = Math.ceil(view / rowHeight) + 1;
    var first = Math.floor(scroll.scrollTop / rowHeight);
    var total = settings.fnRecordsDisplay();
    var start = settings._iDisplayStart;
    var end = settings.fnDisplayEnd();
    var cols = Math.max(1, api.columns(":visible").count());
    var len = Math.max(count * 2, Math.min(count * SCREENS, Math.floor(MAX_CELLS / cols)));
    // Sized once per set of columns, and otherwise only grown: a row height
    // that wobbles by a pixel cannot make the size flip between draws.
    var resize = !sized || settings._iDisplayLength < len;
    if (resize) {
      settings._iDisplayLength = len;
      sized = true;
    }
    len = settings._iDisplayLength;
    // Moves once the view comes within `margin` rows of the window's edge.
    var margin = Math.min(Math.floor(count / 2), Math.floor((len - count) / 4));
    var lo = Math.max(0, first - margin);
    var hi = Math.min(total, first + count + margin);
    if (!resize && lo >= start && hi <= end) return;
    var next = Math.min(first - Math.floor((len - count) / 2), total - len);
    settings._iDisplayStart = Math.max(0, next);
    api.draw("page");
  }

  // After the current event, not inside it: a draw from within DataTables'
  // own draw callbacks would run before it has finished the first.
  function queue() {
    if (queued) return;
    queued = true;
    Promise.resolve().then(update);
  }

  // Columns shown or hidden, or a view of another size, change the row
  // height and how many rows a window can afford.
  function remeasure() {
    rowHeight = 0;
    sized = false;
    queue();
  }

  // DataTables re-measures the header against the drawn rows after every
  // draw, which on a grid hundreds of columns wide costs as much as the draw
  // itself. Column widths are worked out from every row, drawn or not, and
  // columns.adjust() (which a column change runs) still measures at once, so
  // a draw only shifts them when a value outgrows its column. On such a grid
  // the measure waits for a pause in drawing - scrolling, typing - and runs
  // once then.
  settings.aoDrawCallback.forEach(function (callback) {
    if (callback.sName !== "scrolling") return;
    var measureHeader = callback.fn;
    callback.fn = function (s) {
      clearTimeout(resync);
      if (api.columns(":visible").count() < WIDE_COLS) {
        measureHeader(s);
        return;
      }
      resync = setTimeout(function () {
        measureHeader(settings);
      }, SETTLE_MS);
    };
  });

  // A sort or filter starts the table over at its first row while the view
  // stays where it was scrolled to, so every draw re-checks the window.
  api.on("draw.dt", function () {
    pad();
    queue();
  });
  api.on("column-visibility.dt", remeasure);
  scroll.addEventListener("scroll", update, { passive: true });
  // Catches up once an editor that held the window closes without a draw (a
  // value left unchanged).
  $(api.table().body()).on("focusout", function () {
    setTimeout(update, 0);
  });
  new ResizeObserver(remeasure).observe(scroll);

  update();
  // A client-side table draws its first rows as the widget renders, before
  // the page has laid it out, and sizes the header's columns from that draw:
  // they come out narrower than the body's. Measured again once laid out.
  requestAnimationFrame(function () {
    api.columns.adjust();
  });
}

/**
 * Creates the DOM rows a windowed grid has not drawn yet.
 *
 * DT's client-side row selection keeps its state as a class on the row nodes,
 * so a row that has never been drawn could neither show nor keep a selection.
 * Grids that select rows call this on entering selection; cheap once every
 * row exists.
 *
 * @param {object} api DataTables API of the table.
 */
export function createAllRows(api) {
  var settings = api.settings()[0];
  var create = $.fn.dataTable.ext.internal._fnCreateTr;
  for (var i = 0; i < settings.aoData.length; i++) {
    if (!settings.aoData[i].nTr) create(settings, i);
  }
}
