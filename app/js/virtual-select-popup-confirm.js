/**
 * Adds staged Confirm/Cancel semantics to multi-select virtual-select popups[cite: 5].
 *
 * Virtual-select updates selection state immediately on click without staging[cite: 5].
 * This module snapshots current selections when popups open, allowing users to 
 * confirm changes explicitly or revert selection on close (Cancel button, 
 * backdrop click, or Escape key)[cite: 5]. Single-select popups are omitted[cite: 5].
 */

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
  if (!addConfirmButton(container, instance)) return;

  // Custom events do not bubble; attach directly to individual instance containers.
  container.addEventListener("afterOpen", function () {
    confirming.delete(container);
    openSnapshots.set(container, selectedValuesArray(instance));
  });

  container.addEventListener("beforeClose", function () {
    if (confirming.delete(container)) return; // Keep current selection on explicit confirm

    var snapshot = openSnapshots.get(container);
    if (snapshot === undefined) return;

    // Use `setValueMethod` and `renderOptions` instead of `setValue` to ensure internal 
    // `isSelected` flags and DOM option tickmarks are properly updated to match restored state.
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

// Global Escape key handler for body-appended popup dropboxes (bypasses library's default focus check).
document.addEventListener("keydown", function (event) {
  if (event.key !== "Escape") return;
  var openWrapper = document.querySelector(
    ".vscomp-dropbox-wrapper.show-as-popup:not(.closed)"
  );
  if (openWrapper && openWrapper.virtualSelect) {
    openWrapper.virtualSelect.closeDropbox();
  }
});

// Rescan DOM on Shiny reactive updates, Bootstrap modal displays, and page load.
$(document).on("shiny:value", function () {
  setTimeout(scan, 0);
});
$(document).on("shown.bs.modal", function () {
  setTimeout(scan, 0);
});
document.addEventListener("DOMContentLoaded", scan);
scan();