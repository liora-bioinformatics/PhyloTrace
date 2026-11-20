// Wheel zoom and drag-to-pan for the fixed-canvas plots (Tree, Epi, AMR), in
// the viewport zoom_viewport() builds (app/logic/viz_helpers.R). Each plot is
// one pre-rendered PNG, so zooming only resizes that image in the browser and
// never asks the server for anything. Zoom runs from "fit" (the whole plot in
// view) to MAX_NATIVE_SCALE times the image's own pixels; past fit the
// viewport carries `.is-zoomed` and scrolls (see main.scss).

var VIEWPORT_SELECTOR = ".viz-plot-scroll";

// Controls inside a viewport keep their own pointer behaviour.
var IGNORE_SELECTOR = "button, a, input, select, textarea";

// Movement (px) before a press counts as a drag, so a plain click stays a click.
var DRAG_THRESHOLD_PX = 3;

// Furthest zoom, as a multiple of the image's own pixels. The PNG is drawn at
// print resolution, so it stays readable a little past 1:1 before it blurs.
var MAX_NATIVE_SCALE = 2;

// Zoom per pixel of wheel travel. Trackpad pinches arrive as ctrl+wheel with
// much smaller deltas than a mouse notch, hence the separate rate.
var WHEEL_RATE = 0.0015;
var PINCH_RATE = 0.01;
var LINE_PX = 16;

// Per-viewport view: `zoom` is the image width as a multiple of its fit width
// (1 = fit), or null until the viewport's first image has loaded.
var views = new WeakMap();
var pan = null;

var resizeObserver = new ResizeObserver(function (entries) {
  entries.forEach(function (entry) {
    var view = views.get(entry.target);
    if (view && view.zoom > 1) setZoom(entry.target, view.zoom);
  });
});

// Returns the viewport's view record, creating it (and watching its size) on first use.
function viewOf(viewport) {
  var view = views.get(viewport);
  if (!view) {
    view = { zoom: null };
    views.set(viewport, view);
    resizeObserver.observe(viewport);
  }
  return view;
}

// Returns the viewport's plot image once it has pixels to measure, else null.
function plotImage(viewport) {
  var img = viewport.querySelector(".shiny-plot-output img");
  return img && img.naturalWidth > 0 && img.naturalHeight > 0 ? img : null;
}

// Width (CSS px) of the image scaled to fit the viewport, never enlarged past its own pixels.
function fitWidth(viewport, img) {
  var scale = Math.min(
    1,
    viewport.offsetWidth / img.naturalWidth,
    viewport.offsetHeight / img.naturalHeight
  );
  return img.naturalWidth * scale;
}

// Zoom level at which the image is shown at its own pixel size.
function nativeZoom(viewport, img) {
  return img.naturalWidth / fitWidth(viewport, img);
}

// Whether the viewport has anything to scroll to in either direction.
function overflows(viewport) {
  return viewport.scrollWidth > viewport.clientWidth ||
    viewport.scrollHeight > viewport.clientHeight;
}

function updateCanPan(viewport) {
  viewport.classList.toggle("can-pan", overflows(viewport));
}

// Shows the zoom level in the stage's badge, with the gesture that changes it.
function updateBadge(viewport, img) {
  var badge = viewport.parentElement &&
    viewport.parentElement.querySelector(".viz-zoom-badge");
  if (!badge) return;
  var zoom = viewOf(viewport).zoom;
  if (zoom > 1) {
    var percent = Math.round((fitWidth(viewport, img) * zoom * 100) / img.naturalWidth);
    badge.textContent = percent + "% \u00b7 Double-click to fit";
  } else {
    badge.textContent = "Fit \u00b7 Scroll to zoom";
  }
  badge.hidden = false;
}

// Sets a viewport's zoom, keeping the image point under `anchor` (client
// coordinates; the viewport centre by default) where it is on screen.
function setZoom(viewport, zoom, anchor) {
  var view = viewOf(viewport);
  var img = plotImage(viewport);
  if (!img) {
    view.zoom = zoom;
    return;
  }
  var max = Math.max(1, MAX_NATIVE_SCALE * nativeZoom(viewport, img));
  var next = Math.min(max, Math.max(1, zoom));
  view.zoom = next;

  var box = viewport.getBoundingClientRect();
  var at = anchor || { x: box.left + box.width / 2, y: box.top + box.height / 2 };
  var before = img.getBoundingClientRect();
  var rx = Math.min(1, Math.max(0, (at.x - before.left) / before.width));
  var ry = Math.min(1, Math.max(0, (at.y - before.top) / before.height));

  if (next === 1) {
    viewport.classList.remove("is-zoomed");
    img.style.removeProperty("width");
    img.style.removeProperty("height");
  } else {
    var width = fitWidth(viewport, img) * next;
    var height = (width * img.naturalHeight) / img.naturalWidth;
    viewport.classList.add("is-zoomed");
    // Inline !important is the only thing that outranks the fit rules'
    // `width: auto !important` in main.scss.
    img.style.setProperty("width", width + "px", "important");
    img.style.setProperty("height", height + "px", "important");
    var after = img.getBoundingClientRect();
    viewport.scrollLeft += after.left + rx * after.width - at.x;
    viewport.scrollTop += after.top + ry * after.height - at.y;
  }
  updateCanPan(viewport);
  updateBadge(viewport, img);
}

// Returns the viewport a pointer event belongs to, or null for controls.
function viewportFor(target) {
  if (!target || typeof target.closest !== "function") return null;
  if (target.closest(IGNORE_SELECTOR)) return null;
  return target.closest(VIEWPORT_SELECTOR);
}

// Re-applies the view whenever a viewport receives a new image: the first one
// starts at fit, and a re-render (a control change) keeps the reader's zoom
// and roughly where they were looking. Shiny strips the inline size when it
// swaps the image's src, so this has to run on every load. `load` does not
// bubble, hence the capture listener.
document.addEventListener("load", function (e) {
  var img = e.target;
  if (img.tagName !== "IMG") return;
  var viewport = img.closest(VIEWPORT_SELECTOR);
  if (!viewport) return;
  var view = viewOf(viewport);
  if (view.zoom === null) {
    setZoom(viewport, 1);
    return;
  }
  var cx = (viewport.scrollLeft + viewport.clientWidth / 2) / viewport.scrollWidth;
  var cy = (viewport.scrollTop + viewport.clientHeight / 2) / viewport.scrollHeight;
  setZoom(viewport, view.zoom);
  viewport.scrollLeft = cx * viewport.scrollWidth - viewport.clientWidth / 2;
  viewport.scrollTop = cy * viewport.scrollHeight - viewport.clientHeight / 2;
}, true);

// Wheel zooms around the cursor, the way the Map and the MST do.
document.addEventListener("wheel", function (e) {
  var viewport = viewportFor(e.target);
  if (!viewport || !plotImage(viewport)) return;
  e.preventDefault();
  var unit = e.deltaMode === 1 ? LINE_PX : e.deltaMode === 2 ? viewport.clientHeight : 1;
  var factor = Math.exp(-e.deltaY * unit * (e.ctrlKey ? PINCH_RATE : WHEEL_RATE));
  setZoom(viewport, (viewOf(viewport).zoom || 1) * factor, { x: e.clientX, y: e.clientY });
}, { passive: false });

// Double-click toggles between fit and the image's own size at the cursor.
document.addEventListener("dblclick", function (e) {
  var viewport = viewportFor(e.target);
  var img = viewport && plotImage(viewport);
  if (!img) return;
  var zoom = viewOf(viewport).zoom || 1;
  var target = zoom > 1 ? 1 : Math.max(nativeZoom(viewport, img), 2);
  setZoom(viewport, target, { x: e.clientX, y: e.clientY });
});

// Refreshes the grab cursor on entry: the image size changes with each render,
// resize and zoom step.
document.addEventListener("pointerover", function (e) {
  if (!e.target || typeof e.target.closest !== "function") return;
  var viewport = e.target.closest(VIEWPORT_SELECTOR);
  if (viewport) updateCanPan(viewport);
});

// Clears the panning state and the classes it set.
function endPan() {
  if (!pan) return;
  pan.viewport.classList.remove("is-panning");
  if (pan.viewport.hasPointerCapture(pan.pointerId)) {
    pan.viewport.releasePointerCapture(pan.pointerId);
  }
  pan = null;
}

// Starts a pan on a primary-button mouse/pen press inside an overflowing
// viewport. Touch is left alone: the browser already scrolls under a finger.
document.addEventListener("pointerdown", function (e) {
  if (e.button !== 0 || e.pointerType === "touch") return;
  var viewport = viewportFor(e.target);
  if (!viewport || !overflows(viewport)) return;

  // Stops the browser from starting a text selection or an image drag.
  e.preventDefault();
  pan = {
    viewport: viewport,
    pointerId: e.pointerId,
    startX: e.clientX,
    startY: e.clientY,
    scrollLeft: viewport.scrollLeft,
    scrollTop: viewport.scrollTop,
    moved: false
  };
  viewport.setPointerCapture(e.pointerId);
});

// Scrolls the viewport opposite to the pointer, so the image follows the mouse.
document.addEventListener("pointermove", function (e) {
  if (!pan || e.pointerId !== pan.pointerId) return;
  var dx = e.clientX - pan.startX;
  var dy = e.clientY - pan.startY;
  if (!pan.moved) {
    if (Math.abs(dx) < DRAG_THRESHOLD_PX && Math.abs(dy) < DRAG_THRESHOLD_PX) return;
    pan.moved = true;
    pan.viewport.classList.add("is-panning");
  }
  pan.viewport.scrollLeft = pan.scrollLeft - dx;
  pan.viewport.scrollTop = pan.scrollTop - dy;
});

document.addEventListener("pointerup", endPan);
document.addEventListener("pointercancel", endPan);

// Capture can be lost without a pointerup (e.g. the stage re-renders mid-drag).
document.addEventListener("lostpointercapture", function (e) {
  if (pan && e.pointerId === pan.pointerId) endPan();
}, true);

// Without this, dragging the plot <img> pulls out a ghost copy instead of panning.
document.addEventListener("dragstart", function (e) {
  if (viewportFor(e.target)) e.preventDefault();
});
