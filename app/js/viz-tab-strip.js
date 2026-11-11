// Visualization plot tab strip (app/view/visualization.R's `plot_set`): a single
// scrolling row (see main.scss). Keeps the selected tab in view, maps vertical
// wheel movement to horizontal scrolling, and flags hidden overflow on either
// side so the stylesheet can hint at it.

var STRIP_ID = "app-visualization-plot_set";

function getStrip() {
  return document.getElementById(STRIP_ID);
}

// Toggles the classes that mark tabs hidden to the left or right of the strip.
function updateOverflow(strip) {
  var max = strip.scrollWidth - strip.clientWidth;
  strip.classList.toggle("viz-strip-more-left", strip.scrollLeft > 1);
  strip.classList.toggle("viz-strip-more-right", strip.scrollLeft < max - 1);
}

// Width of the right-edge fade (2.5rem in main.scss), kept clear when revealing.
var FADE_PX = 40;

// Scrolls the strip just far enough to show a tab clear of the pinned "New" tab
// and of the right-edge fade.
function revealTab(strip, link) {
  var li = link.closest("li");
  if (!li || li === strip.firstElementChild) return;
  var pinned = strip.firstElementChild ? strip.firstElementChild.offsetWidth : 0;
  var left = li.offsetLeft - strip.offsetLeft;
  var right = left + li.offsetWidth;
  if (left - pinned < strip.scrollLeft) {
    strip.scrollLeft = left - pinned;
  } else if (right + FADE_PX > strip.scrollLeft + strip.clientWidth) {
    strip.scrollLeft = right + FADE_PX - strip.clientWidth;
  }
  updateOverflow(strip);
}

// Fires for clicks, nav_select() and nav_insert(select = TRUE) alike.
$(document).on("shown.bs.tab", "#" + STRIP_ID + " [data-bs-toggle='tab']", function (e) {
  var strip = getStrip();
  if (strip) revealTab(strip, e.target);
});

// A plain mouse wheel only scrolls vertically; turn that into sideways motion
// over the strip. Trackpad horizontal swipes are left to the browser.
document.addEventListener(
  "wheel",
  function (e) {
    var strip = e.target.closest && e.target.closest("#" + STRIP_ID);
    if (!strip || strip.scrollWidth <= strip.clientWidth) return;
    if (Math.abs(e.deltaY) <= Math.abs(e.deltaX)) return;
    e.preventDefault();
    strip.scrollLeft += e.deltaY;
  },
  { passive: false }
);

document.addEventListener(
  "scroll",
  function (e) {
    if (e.target && e.target.id === STRIP_ID) updateOverflow(e.target);
  },
  true
);

// Tabs come and go via nav_insert/nav_remove and the window can resize, so
// re-check the overflow hints whenever the strip's contents or size change.
var observed = null;
var resizeObserver = new ResizeObserver(function () {
  if (observed) updateOverflow(observed);
});
var mutationObserver = new MutationObserver(function () {
  if (observed) updateOverflow(observed);
});

// The strip is re-rendered on every database load, so attach to each new copy.
new MutationObserver(function () {
  var strip = getStrip();
  if (strip === observed) return;
  resizeObserver.disconnect();
  mutationObserver.disconnect();
  observed = strip;
  if (!strip) return;
  resizeObserver.observe(strip);
  mutationObserver.observe(strip, { childList: true });
  updateOverflow(strip);
}).observe(document.body, { childList: true, subtree: true });
