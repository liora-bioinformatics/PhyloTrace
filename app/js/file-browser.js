/**
 * Client side of the app's own file dialog (app/view/file_dialog.R): an
 * overlay of its own, outside Shiny's single modal slot, so it can open over
 * another dialog.
 *
 * Rows and navigation targets carry only an index and the Shiny input to report
 * to; the server keeps the listing itself, so no path ever travels through
 * markup or script. Selection is highlighted here, without a round trip, which
 * keeps folders with thousands of entries responsive. Every report names the
 * listing it was made in (`data-no`), so the server can ignore one from a
 * folder that has been left since.
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

// Row a Shift-click extends the selection from, per listing element.
const anchors = new WeakMap();

function selectRange(list, from, to) {
  const rows = Array.prototype.slice.call(list.querySelectorAll(".fb-row"));
  const a = rows.indexOf(from);
  const b = rows.indexOf(to);
  rows.forEach((row, k) => {
    row.classList.toggle("is-selected", k >= Math.min(a, b) && k <= Math.max(a, b));
  });
}

document.addEventListener("click", (event) => {
  const row = event.target.closest(".fb-row");
  // Files in a folder picker are shown for orientation only.
  if (row && row.classList.contains("is-inert")) return;
  if (row) {
    const list = row.closest(".fb-list");
    const multiple = list.dataset.multiple === "true";
    const anchor = anchors.get(list);
    if (multiple && event.shiftKey && anchor && list.contains(anchor)) {
      selectRange(list, anchor, row);
    } else if (multiple && (event.ctrlKey || event.metaKey)) {
      row.classList.toggle("is-selected");
      anchors.set(list, row);
    } else {
      list.querySelectorAll(".fb-row.is-selected").forEach((other) => {
        if (other !== row) other.classList.remove("is-selected");
      });
      row.classList.add("is-selected");
      anchors.set(list, row);
    }
    report(`${list.dataset.input}_selected`, {
      no: Number(list.dataset.no),
      i: selectedIndices(list),
    });
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
  if (!row || row.classList.contains("is-inert")) return;
  const list = row.closest(".fb-list");
  report(`${list.dataset.input}_open`, {
    no: Number(list.dataset.no),
    i: Number(row.dataset.i),
    t: Date.now(),
  });
});

// Enter in the new-folder or file-name field presses the button next to it.
document.addEventListener("keydown", (event) => {
  if (event.key !== "Enter") return;
  const field = event.target.closest("[data-enter-click]");
  if (!field) return;
  const button = document.getElementById(field.dataset.enterClick);
  if (button) {
    event.preventDefault();
    // Text inputs report with a delay; send the typed value before the click,
    // or the server would act on the value from before the last keystrokes.
    if (event.target.id) report(event.target.id, event.target.value);
    button.click();
  }
});

// A Bootstrap dialog open underneath traps focus by watching focus events on
// the document and pulling focus back into itself. Focus events from inside
// this dialog are kept from reaching the document, so no trap - however and
// whenever it was switched on - sees focus leave its own dialog.
function shieldFocus(overlay) {
  if (overlay.dataset.focusShield) return;
  overlay.dataset.focusShield = "1";
  overlay.addEventListener("focusin", (event) => event.stopPropagation());
}

function openDialogs() {
  return document.querySelectorAll(".fb-overlay.is-open");
}

// Shiny pauses outputs it sees as hidden and only re-checks on a "shown" or
// "hidden" event; showing the overlay by a class change must announce itself,
// or its listing can stay paused at whatever it showed before.
function announceVisibility(overlay, show) {
  if (window.jQuery) window.jQuery(overlay).trigger(show ? "shown" : "hidden");
}

function setDialog(overlay, show, focusId) {
  if (!overlay) return;
  if (show) {
    shieldFocus(overlay);
    overlay.classList.add("is-open");
    document.documentElement.classList.add("fb-open");
    announceVisibility(overlay, true);
    const field = focusId && document.getElementById(focusId);
    if (field) setTimeout(() => field.focus(), 50);
  } else {
    overlay.classList.remove("is-open");
    announceVisibility(overlay, false);
    if (!openDialogs().length) {
      document.documentElement.classList.remove("fb-open");
    }
  }
}

Shiny.addCustomMessageHandler("fb-dialog", (msg) => {
  setDialog(document.getElementById(msg.id), msg.show, msg.focus);
});

// Cancel and the header's close button close the dialog; nothing is chosen.
document.addEventListener("click", (event) => {
  const close = event.target.closest(".fb-close");
  if (close) setDialog(close.closest(".fb-overlay"), false);
});

// Escape closes the topmost open dialog.
document.addEventListener("keydown", (event) => {
  if (event.key !== "Escape") return;
  const open = openDialogs();
  if (open.length) {
    event.stopPropagation();
    setDialog(open[open.length - 1], false);
  }
});
