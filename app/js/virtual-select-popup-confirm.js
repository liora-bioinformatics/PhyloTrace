// Give the *multi-select* modal virtualSelectInput popups (showDropboxAsPopup
// = TRUE in database_browser.R / database_custom.R) an explicit Confirm button
// next to the library's own close button.
//
// virtual-select applies every checkbox click to the underlying value as soon
// as it happens - there is no staged/pending selection to "confirm". What the
// close (X) button actually does today is just dismiss the popup, which reads
// as confirming by accident: there is no way to back out of a multi-select
// session once you have started ticking boxes. This module gives the X real
// Cancel semantics (revert to whatever was selected when the popup opened)
// and adds a checkmark button that keeps the current selection. Clicking the
// backdrop cancels too, since that also raises "beforeClose".
//
// Single-select popups (scheme_browser.R's scheme_selector) are deliberately
// left alone: picking an option there is one click that already closes the
// popup by itself, so there is nothing to confirm and no partial state to back
// out of. A confirm/cancel pair would only add a step to a one-click gesture.
//
// Scoped to `.vscomp-wrapper.show-as-popup`, the class virtual-select itself
// adds only when `showDropboxAsPopup` resolves to true (see
// popupDropboxBreakpoint in the vendored library) - plain (non-popup)
// virtualSelectInputs are untouched.

// Keyed on the outer `#<inputId>.virtual-select` container so a Shiny
// re-render (which replaces that element) starts fresh rather than being
// mistaken for one we have already wired up.
var bound = new WeakSet();
var openSnapshots = new WeakMap();
var confirming = new WeakSet();

function selectedValuesArray(instance) {
  var values = instance.selectedValues;
  return Array.isArray(values) ? values.slice() : values;
}

function addConfirmButton(container, instance) {
  var closeButton = instance.$dropboxCloseButton;
  if (!closeButton || !closeButton.parentElement) return false;

  // Marks the dropbox as carrying the button pair, so the stylesheet can shift
  // the close button off center only where a confirm button sits beside it.
  closeButton.parentElement.classList.add("pt-has-confirm");

  closeButton.title = "Cancel";
  closeButton.setAttribute("aria-label", "Cancel");

  var confirmButton = document.createElement("span");
  confirmButton.className = "vscomp-dropbox-confirm-button";
  confirmButton.title = "Confirm";
  confirmButton.setAttribute("role", "button");
  confirmButton.setAttribute("tabindex", "0");
  confirmButton.setAttribute("aria-label", "Confirm");
  confirmButton.innerHTML = '<i class="vscomp-confirm-icon"></i>';

  function confirmAndClose() {
    confirming.add(container);
    instance.closeDropbox();
  }

  confirmButton.addEventListener("click", confirmAndClose);
  confirmButton.addEventListener("keydown", function (event) {
    if (event.key !== "Enter" && event.key !== " ") return;
    event.preventDefault();
    confirmAndClose();
  });

  closeButton.insertAdjacentElement("afterend", confirmButton);
  return true;
}

function bind(container) {
  if (bound.has(container)) return;
  var instance = container.virtualSelect;
  if (!instance || !instance.multiple) return;

  bound.add(container);
  // Without a confirm button there is no way to keep a selection, so the
  // revert-on-close listeners below must not be attached either.
  if (!addConfirmButton(container, instance)) return;

  // Non-bubbling by design (virtual-select dispatches them straight on the
  // element passed to init), so these must be bound per instance rather than
  // delegated from document.
  container.addEventListener("afterOpen", function () {
    confirming.delete(container);
    openSnapshots.set(container, selectedValuesArray(instance));
  });

  container.addEventListener("beforeClose", function () {
    if (confirming.delete(container)) return; // confirmed - keep current value
    var snapshot = openSnapshots.get(container);
    if (snapshot === undefined) return;
    // setValue() is the low-level setter: it rewrites `selectedValues` and the
    // toggle's label but leaves each option's `isSelected` flag - and the
    // already-rendered `.vscomp-option.selected` rows - exactly as the
    // cancelled session left them. Reopening does not repaint the list either
    // (openDropbox only re-renders when showSelectedOptionsFirst is on), so the
    // ticks would outlive the value they came from. Worse, selectOption() reads
    // tick-vs-untick off the row's class while pushing onto `selectedValues`,
    // so a row the revert deselected can be re-ticked into a duplicate value.
    // setValueMethod() re-derives every `isSelected` from the value it is
    // given, and renderOptions() repaints the rows from those flags.
    instance.setValueMethod(snapshot);
    instance.renderOptions();
  });
}

function scan() {
  document.querySelectorAll(".virtual-select").forEach(function (container) {
    var wrapper = container.querySelector(".vscomp-wrapper");
    if (wrapper && wrapper.classList.contains("show-as-popup")) bind(container);
  });
}

// Escape has to be handled here because the library's own handler cannot fire
// for these popups. It gates on `$wrapper.contains(document.activeElement)`,
// but `$wrapper` is the inline control while `showDropboxAsPopup` renders the
// options into the body-appended `$dropboxWrapper` (dropboxWrapper = "body").
// Focus therefore always sits outside the element it tests, and Escape is a
// dead key on every popup in the app. Closing through closeDropbox() raises
// "beforeClose", so multi-select popups revert via the listener above and
// single-select ones simply close.
document.addEventListener("keydown", function (event) {
  if (event.key !== "Escape") return;
  var openWrapper = document.querySelector(
    ".vscomp-dropbox-wrapper.show-as-popup:not(.closed)"
  );
  if (openWrapper && openWrapper.virtualSelect) {
    openWrapper.virtualSelect.closeDropbox();
  }
});

$(document).on("shiny:value", function () {
  setTimeout(scan, 0);
});
$(document).on("shown.bs.modal", function () {
  setTimeout(scan, 0);
});
document.addEventListener("DOMContentLoaded", scan);
scan();
