// Block pointer input for the duration of a user-triggered server round trip.
//
// R is single-threaded: while an event handler runs, the browser stays fully
// interactive and every further click is queued and replayed the moment R frees
// up — a burst of handlers running against state the user no longer expects.
// This shield rejects those clicks at the source.
//
// It cannot be driven from inside the R handlers themselves: the click that
// overflows is fired *during* the round trip, before the handler body starts, so
// a lock engaged there always trails the problem. Instead the shield engages on
// the client the instant a gesture pushes an input to the server, and holds
// while Shiny reports itself busy.
//
// The trigger is deliberately "did a user gesture cause this?", not "is the
// server busy?". Shiny sets `shiny-busy` for *any* server activity, including
// the 700ms typing progress poll and the 750ms Map/Epi animation ticks — locking
// on that alone would hold the page for the whole run and make Terminate and
// Pause unreachable.

var GESTURE_WINDOW_MS = 400; // how recent a gesture must be to count as the cause
var BUSY_PROBE_MS = 250; // grace before we insist the server actually got busy
var IDLE_GRACE_MS = 150; // bridges chains that span several reactive cycles
var MAX_HOLD_MS = 10 * 60 * 1000; // last-resort failsafe

var engaged = false;
var pointerDown = false;
var lastGestureAt = 0;
var probeTimer = null;
var graceTimer = null;
var failsafeTimer = null;
var shieldEl = null;

// Interactions that must never engage the shield, because locking them breaks
// the interaction itself rather than protecting it.
function isTypingSurface(el) {
  if (el.tagName === "TEXTAREA") return true;
  if (el.tagName !== "INPUT") return false;
  return el.type === "text" || el.type === "search";
}

// A pickerInput bound to a `multiple` select: the menu stays open and the user
// keeps clicking options. Single-select pickers are NOT exempt — committing one
// is a discrete choice that usually drives an expensive rebuild.
function isMultiSelectMenu(el) {
  var wrapper = el.closest(".bootstrap-select");
  return Boolean(wrapper && wrapper.querySelector("select[multiple]"));
}

function isExemptFromShield(el) {
  if (!el || typeof el.closest !== "function") return true;

  // Deliberately NOT the whole `.dropdown-menu`: that would also cover the
  // option items, and choosing a value from a pickerInput is exactly the kind of
  // gesture the shield exists for. Only the search *field* inside it is exempt,
  // which the typing-surface test below already covers — along with the DT
  // filter box and every textInput.
  //
  // The three widget families after the first group are exempt for one shared
  // reason: their normal use is a *rapid repeated* interaction, and blocking
  // optimistically on the first click swallows the second. A DT cell sends
  // `_cell_clicked` on the opening click of a double-click, so shielding there
  // means the dblclick never lands and inline editing silently stops working.
  return Boolean(
    isTypingSurface(el) ||
    el.closest(".time-step-buttons") || // Map/Epi play + step transport
    el.closest(".pt-no-lock") ||        // generic opt-out (typing Terminate)
    el.closest(".leaflet") ||           // map pan-zoom streams inputs
    el.closest(".vis-network") ||       // MST node drags stream inputs
    el.closest(".pickr") ||             // colour picker updates while dragging
    el.closest("table.dataTable tbody") || // dblclick-to-edit, row selection
    isMultiSelectMenu(el) ||            // menu stays open across picks
    (el.tagName === "INPUT" && el.type === "number") // stepper arrows
  );
  // Sliders are deliberately absent: the pointerDown guard already suppresses
  // the values ionRangeSlider streams mid-drag, and exempting `.irs` outright
  // would also skip the replot on release — the expensive half.
}

function clearTimers() {
  if (probeTimer) { clearTimeout(probeTimer); probeTimer = null; }
  if (graceTimer) { clearTimeout(graceTimer); graceTimer = null; }
  if (failsafeTimer) { clearTimeout(failsafeTimer); failsafeTimer = null; }
}

function serverIsBusy() {
  return document.documentElement.classList.contains("shiny-busy");
}

function release() {
  if (!engaged) return;
  engaged = false;
  clearTimers();
  document.documentElement.classList.remove("pt-busy", "pt-busy-visible");
}

function engage() {
  if (engaged) return;
  engaged = true;

  if (!shieldEl) {
    shieldEl = document.createElement("div");
    shieldEl.className = "pt-shield";
    document.body.appendChild(shieldEl);
  }

  clearTimers();
  document.documentElement.classList.add("pt-busy");

  // Blocking is immediate, but the wait cursor is delayed: most handlers finish
  // well inside BUSY_PROBE_MS and a cursor flip that fast only reads as flicker.
  // The same timer doubles as the "did this input actually reach the server?"
  // check — a gesture that caused no server work releases here.
  probeTimer = setTimeout(function () {
    probeTimer = null;
    // State, not the shiny:busy event: if R was *already* busy when this input
    // landed, incrementBusyCount() does not re-send `busy` (it only fires on the
    // 0 -> 1 transition), so an event-based check would wrongly release.
    if (serverIsBusy()) {
      document.documentElement.classList.add("pt-busy-visible");
    } else {
      release();
    }
  }, BUSY_PROBE_MS);

  failsafeTimer = setTimeout(function () {
    failsafeTimer = null;
    // eslint-disable-next-line no-console
    console.warn("busy-shield: held past MAX_HOLD_MS, releasing");
    release();
  }, MAX_HOLD_MS);
}

document.addEventListener("pointerdown", function () {
  pointerDown = true;
}, true);

// Arm on pointer *up*, so a drag — a slider handle, a map pan, a network node —
// only counts once it has finished. Combined with the pointerDown guard below,
// this exempts every drag interaction without having to enumerate them.
document.addEventListener("pointerup", function (event) {
  pointerDown = false;
  if (!isExemptFromShield(event.target)) lastGestureAt = Date.now();
}, true);

// Enter/Space only. Plain keystrokes never arm, which is what keeps text and
// search fields usable while they push values to the server per character.
document.addEventListener("keyup", function (event) {
  if (event.key !== "Enter" && event.key !== " ") return;
  if (!isExemptFromShield(event.target)) lastGestureAt = Date.now();
}, true);

document.addEventListener("keydown", function (event) {
  if (!engaged) return;
  if (event.key === "Escape") {
    release(); // escape hatch
    return;
  }
  // Keyboard is not covered by pointer-events, so a focused button could still
  // be re-triggered. Swallow the keys that would do it. Deliberately no blur():
  // that fires a change event and starts another round trip.
  if (event.key === "Enter" || event.key === " ") {
    event.preventDefault();
    event.stopPropagation();
  }
}, true);

// Keyed to the input change rather than the raw click: it is the actual cause of
// server work, and requiring a recent gesture excludes programmatic cascades
// (the animation loops' updateSliderInput) and timer-driven traffic.
$(document).on("shiny:inputchanged", function () {
  if (pointerDown) return;
  if (Date.now() - lastGestureAt > GESTURE_WINDOW_MS) return;
  engage();
});

$(document).on("shiny:busy", function () {
  if (graceTimer) { clearTimeout(graceTimer); graceTimer = null; }
});

$(document).on("shiny:idle", function () {
  if (!engaged || graceTimer) return;
  graceTimer = setTimeout(function () {
    graceTimer = null;
    if (!serverIsBusy()) release();
  }, IDLE_GRACE_MS);
});

// Shiny puts up its own overlay from here on.
$(document).on("shiny:disconnected", release);
