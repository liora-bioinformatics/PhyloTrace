/**
 * Places the overview tables' range-slider filter popups and keeps the table
 * body in line with its header.
 *
 * DT hangs a slider popup below its input and lifts the clipping of the
 * header (and forces a scrollbar on the scroll wrapper) so it can show; the
 * overview tables keep the header clipped instead (`.loci-table-body` in
 * main.scss), so the popup is fixed to the window right below its input, and
 * moved left where it would stick out of the table.
 *
 * Focusing a filter input that is scrolled out of view scrolls the header to
 * it, but DataTables only ever moves the header after the body, so the
 * columns came apart: the body is brought along.
 */

var SCOPE = ".loci-table-body";

// Lines the body up with a header the browser scrolled to a focused input.
document.addEventListener(
  "scroll",
  function (event) {
    var head = event.target;
    if (!head.classList || !head.classList.contains("dataTables_scrollHead")) return;
    if (!head.closest(SCOPE)) return;
    var body = head.parentNode.querySelector(".dataTables_scrollBody");
    if (body && body.scrollLeft !== head.scrollLeft) body.scrollLeft = head.scrollLeft;
  },
  true,
);

// Fixes an opened slider popup below its input. `focusin` follows the `focus`
// handler in which DT shows and sizes the popup.
document.addEventListener("focusin", function (event) {
  var input = event.target;
  if (input.tagName !== "INPUT" || !input.closest(SCOPE)) return;
  var cell = input.closest(".dataTables_scrollHead td[data-type]");
  var popup = cell ? cell.lastElementChild : null;
  if (!popup || popup === input.parentNode || !popup.querySelector(".noUi-target")) return;
  var field = input.getBoundingClientRect();
  var table = cell.closest(".dataTables_scroll").getBoundingClientRect();
  var right = Math.min(table.right, window.innerWidth);
  popup.style.position = "fixed";
  popup.style.top = field.bottom + "px";
  popup.style.left = Math.max(table.left, Math.min(field.left, right - popup.offsetWidth)) + "px";
});
