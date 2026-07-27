// Give the modal-style virtualSelectInput popups (showDropboxAsPopup = TRUE in
// database_browser.R / database_custom.R / scheme_browser.R) an explicit
// Confirm button next to the library's own close button.
//
// virtual-select applies every checkbox click to the underlying value as soon
// as it happens - there is no staged/pending selection to "confirm". What the
// close (X) button actually does today is just dismiss the popup, which reads
// as confirming by accident: there is no way to back out of a multi-select
// session once you have started ticking boxes. This module gives the X real
// Cancel semantics (revert to whatever was selected when the popup opened)
// and adds a checkmark button that keeps the current selection - covering
// Escape and outside-clicks too, since both also raise "beforeClose".
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
  if (!closeButton || !closeButton.parentElement) return;

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
}

function bind(container) {
  if (bound.has(container)) return;
  var instance = container.virtualSelect;
  if (!instance) return;

  bound.add(container);
  addConfirmButton(container, instance);

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
    if (snapshot !== undefined) instance.setValue(snapshot);
  });
}

function scan() {
  document.querySelectorAll(".virtual-select").forEach(function (container) {
    var wrapper = container.querySelector(".vscomp-wrapper");
    if (wrapper && wrapper.classList.contains("show-as-popup")) bind(container);
  });
}

$(document).on("shiny:value", function () {
  setTimeout(scan, 0);
});
$(document).on("shown.bs.modal", function () {
  setTimeout(scan, 0);
});
document.addEventListener("DOMContentLoaded", scan);
scan();
