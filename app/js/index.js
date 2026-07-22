import "./busy-shield";

// Restrict free-text entry to a filesystem-/SQL-safe character set
// ([a-zA-Z0-9_-]) so anything the user types to *name* an identifier (a
// database, a scheme, an isolate) stays safe to persist, export as a filename,
// and query with.
//
// It must NOT touch inputs where symbols and spaces are legitimate and
// expected, which the exemptions below carve out:
//   * colour-picker hex fields (which need "#"),
//   * the search boxes inside select/picker dropdowns (a query, not a name),
//   * anything a user is *labelling* rather than naming an identifier — plot
//     titles, subtitles, annotation labels, and the analysis/plot names on the
//     dashboard — which are display text and should take spaces and punctuation
//     freely. Those inputs opt out by sitting inside a ".allow-free-text"
//     wrapper (see the visualization and analysis_dashboard submodules).
function isExemptFromCharset(el) {
  return Boolean(
    el.classList.contains("pcr-result") || // colour-picker hex field
    el.closest(".pickr") ||                 // anywhere inside a colour picker
    el.closest(".bs-searchbox") ||          // pickerInput live-search box
    el.closest(".vscomp-dropbox") ||        // virtualSelectInput search box
    el.closest(".selectize-control") ||     // selectize search / entry
    el.closest(".dropdown-menu") ||         // any dropdown's own search field
    el.closest(".allow-free-text")          // plot labels/titles: display text
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
