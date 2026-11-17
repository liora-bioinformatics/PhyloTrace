/**
 * Client side of the app's own file dialog (app/view/file_dialog.R).
 *
 * Rows and navigation targets carry only an index and the Shiny input to report
 * to; the server keeps the listing itself, so no path ever travels through
 * markup or script. Selection is highlighted here, without a round trip, which
 * keeps folders with thousands of entries responsive.
 */

function report(inputId, value) {
  if (window.Shiny && inputId) {
    window.Shiny.setInputValue(inputId, value, { priority: "event" });
  }
}

function selectedIndices(list) {
  return Array.prototype.map.call(
    list.querySelectorAll(".fb-row.is-selected"),
    (row) => Number(row.dataset.i),
  );
}

document.addEventListener("click", (event) => {
  const row = event.target.closest(".fb-row");
  if (row) {
    const list = row.closest(".fb-list");
    const multiple = list.dataset.multiple === "true";
    const additive = multiple && (event.ctrlKey || event.metaKey);
    if (additive) {
      row.classList.toggle("is-selected");
    } else {
      list.querySelectorAll(".fb-row.is-selected").forEach((other) => {
        if (other !== row) other.classList.remove("is-selected");
      });
      row.classList.add("is-selected");
    }
    report(`${list.dataset.input}_selected`, selectedIndices(list));
    return;
  }

  const nav = event.target.closest(".fb-nav");
  if (nav) {
    event.preventDefault();
    report(nav.dataset.input, {
      kind: nav.dataset.kind,
      i: Number(nav.dataset.i),
      t: Date.now(),
    });
  }
});

document.addEventListener("dblclick", (event) => {
  const row = event.target.closest(".fb-row");
  if (!row) return;
  const list = row.closest(".fb-list");
  report(`${list.dataset.input}_open`, { i: Number(row.dataset.i), t: Date.now() });
});
