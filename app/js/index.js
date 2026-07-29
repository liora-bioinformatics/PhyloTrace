import "./busy-shield";
import "./dt-column-sizing";
import "./dt-column-visibility";
import "./virtual-select-popup-confirm";

/**
 * Determines whether an input element is exempt from character set sanitization.
 *
 * Exempts search filters, color pickers, and explicit display text containers (.allow-free-text)
 * where spaces, punctuation, or special characters are required.
 */
function isExemptFromCharset(el) {
  return Boolean(
    el.classList.contains("pcr-result") || // Color picker hex value
    el.closest(".pickr") ||                 // Color picker component container
    el.closest(".bs-searchbox") ||          // Bootstrap-select search filter
    el.closest(".vscomp-dropbox") ||        // Virtual-select search filter
    el.closest(".selectize-control") ||     // Selectize input filter
    el.closest(".dropdown-menu") ||         // Generic dropdown search field
    el.closest(".allow-free-text")          // Opt-out wrapper for display text (e.g., plot titles/labels)
  );
}

// Restrict default text inputs to filesystem and SQL-safe characters ([a-zA-Z0-9_-]).
document.addEventListener("input", function (event) {
  var el = event.target;
  var isText = el.tagName === "INPUT" && el.type === "text";
  var isHot = el.classList.contains("handsontableInput");
  if (!(isText || isHot)) return;
  if (isExemptFromCharset(el)) return;

  var regex = /^[a-zA-Z0-9_-]*$/;
  if (!regex.test(el.value)) {
    el.value = el.value.replace(/[^a-zA-Z0-9_-]/g, "");
  }
});

// Converts single-click interactions into double-clicks to enable immediate cell editing in DataTables.
$(document).on("click", ".edit-table table.dataTable tbody td", function (e) {
  if (e.target !== this) return; // Ignore clicks targetting active cell editors
  $(this).trigger("dblclick");
});

// Fades out and cleans up the initial splash loading screen upon initial Shiny idle.
$(document).one("shiny:idle", function () {
  setTimeout(function () {
    var overlay = document.querySelector(".waiter-overlay");
    if (!overlay) return;
    overlay.style.transition = "opacity 0.4s ease";
    overlay.style.opacity = "0";
    setTimeout(function () { overlay.remove(); }, 400);
  }, 1500);
});