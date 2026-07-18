// Restrict free-text entry to a filesystem-/SQL-safe character set
// ([a-zA-Z0-9_-]) so anything the user types to *name* or *label* something
// stays safe to persist, export as a filename, and feed to ggplot's plotmath.
//
// This is a labelling restriction, not a blanket one: it must NOT touch inputs
// where symbols and spaces are legitimate and expected — colour-picker hex
// fields (which need "#") and the search boxes inside select/picker dropdowns
// (where you type a query, not a label). Those are skipped below, which is what
// scopes the rule to "text the user is labelling something with" across the
// visualization submodules and the rest of the app alike.
function isExemptFromCharset(el) {
  return Boolean(
    el.classList.contains("pcr-result") || // colour-picker hex field
    el.closest(".pickr") ||                 // anywhere inside a colour picker
    el.closest(".bs-searchbox") ||          // pickerInput live-search box
    el.closest(".selectize-control") ||     // selectize search / entry
    el.closest(".dropdown-menu")            // any dropdown's own search field
  );
}

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

$(document).one("shiny:idle", function () {
  setTimeout(function () {
    var overlay = document.querySelector(".waiter-overlay");
    if (!overlay) return;
    overlay.style.transition = "opacity 0.4s ease";
    overlay.style.opacity = "0";
    setTimeout(function () { overlay.remove(); }, 400);
  }, 1500);
});
