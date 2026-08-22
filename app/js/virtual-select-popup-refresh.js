/**
 * Makes a virtual-select popup show its options the moment it opens.
 *
 * virtual-select is a windowing list: it renders a slice of the options and
 * translates that slice to wherever the scroll position says it belongs. The
 * two are only kept in step by its own scroll handler, so anything that moves
 * the options container without going through that handler — a re-render while
 * the box was scrolled, a body-appended dropbox outliving the input that owned
 * it — leaves the rendered slice parked somewhere outside the viewport. The
 * popup then opens on an empty white box, and the first scroll fixes it, which
 * is exactly what a user sees and cannot explain.
 *
 * Re-measuring and re-rendering on open costs one frame and settles it whatever
 * left it that way. The scroll reset is not incidental: a picker should open at
 * the top of its list, and starting there is also what guarantees the slice and
 * the scroll position agree.
 */

var bound = new WeakSet();

function refresh(instance) {
  if (!instance || !instance.$optionsContainer) return;
  instance.$optionsContainer.scrollTop = 0;
  // The popup sizes its window from the viewport rather than from the
  // `optionsCount` prop, so this is also what corrects a box opened at one
  // window height and used at another.
  if (instance.setOptionsContainerHeight) instance.setOptionsContainerHeight(true);
  if (instance.setVisibleOptions) instance.setVisibleOptions();
}

function bind(container) {
  if (bound.has(container)) return;
  var instance = container.virtualSelect;
  if (!instance) return;

  bound.add(container);
  // Custom events do not bubble; attach directly to individual instance containers.
  container.addEventListener("afterOpen", function () {
    // After the frame the popup is laid out in — measuring it before that
    // reads the closed box's height.
    requestAnimationFrame(function () { refresh(instance); });
  });
}

function scan() {
  document.querySelectorAll(".virtual-select").forEach(function (container) {
    var wrapper = container.querySelector(".vscomp-wrapper");
    if (wrapper && wrapper.classList.contains("show-as-popup")) bind(container);
  });
}

// Rescan DOM on Shiny reactive updates, Bootstrap modal displays, and page load.
$(document).on("shiny:value", function () {
  setTimeout(scan, 0);
});
$(document).on("shown.bs.modal", function () {
  setTimeout(scan, 0);
});
document.addEventListener("DOMContentLoaded", scan);
scan();
