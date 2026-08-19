/**
 * Input Shield for Shiny Applications
 *
 * Prevents UI interaction during server-side processing to eliminate queued click 
 * bursts caused by R's single-threaded nature.
 *
 * Design Note:
 * Engages immediately upon client-side user gestures rather than relying solely on 
 * `shiny-busy` (which also fires during background polling/animations where full 
 * UI locking is unwanted).
 */

var GESTURE_WINDOW_MS = 400;      // Max time window between user gesture and input change
var BUSY_PROBE_MS = 250;          // Grace period before displaying busy cursor and verifying server state
var IDLE_GRACE_MS = 150;          // Bridges rapid back-to-back reactive updates
var MAX_HOLD_MS = 10 * 60 * 1000; // Failsafe timeout to prevent permanent UI lockup

var engaged = false;
var pointerDown = false;
var lastGestureAt = 0;
var probeTimer = null;
var graceTimer = null;
var failsafeTimer = null;
var holdFailsafeTimer = null;
var shieldEl = null;

function isTypingSurface(el) {
  if (el.tagName === "TEXTAREA") return true;
  if (el.tagName !== "INPUT") return false;
  return el.type === "text" || el.type === "search";
}

// Exempt multi-select pickers as users select multiple items while keeping the menu open.
function isMultiSelectMenu(el) {
  var wrapper = el.closest(".bootstrap-select");
  return Boolean(wrapper && wrapper.querySelector("select[multiple]"));
}

// Exempts interactive elements where immediate blocking breaks natural usage (e.g., rapid clicks, drags, typing).
function isExemptFromShield(el) {
  if (!el || typeof el.closest !== "function") return true;

  // Note: Sliders (`.irs`) are implicitly handled via `pointerDown` state tracking.
  return Boolean(
    isTypingSurface(el) ||
    el.closest(".time-step-buttons") ||        // Transport controls (play/step)
    el.closest(".pt-no-lock") ||               // Generic opt-out hook
    el.closest(".leaflet") ||                  // Map panning/zooming
    el.closest(".vis-network") ||              // Graph node dragging
    el.closest(".pickr") ||                    // Color picker dragging
    el.closest("table.dataTable tbody") ||     // Row selection and dblclick inline edits
    isMultiSelectMenu(el) ||
    (el.tagName === "INPUT" && el.type === "number") || // Stepper arrows
    // Tab/pill navigation (e.g. the top-level nav bar, bslib accordions):
    // switching away must stay possible even while a long-running,
    // continuously-busy operation (e.g. typing's pre-run genome check) has
    // Shiny too busy to ever fire shiny:idle - without this, clicking the
    // trigger arms the shield on the resulting input change and it cannot
    // release until that operation finally ends.
    el.closest('[data-bs-toggle="tab"]') ||
    el.closest('[data-bs-toggle="collapse"]')
  );
}

// Controls that stay live through a server-driven hold (see holdShield).
function isShieldPassthrough(el) {
  return Boolean(
    el && typeof el.closest === "function" && el.closest(".pt-shield-passthrough")
  );
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

  // Delay the visual wait cursor to prevent flicker on rapid round-trips.
  // Checks DOM state directly because `shiny:busy` only triggers on 0 -> 1 count transitions.
  probeTimer = setTimeout(function () {
    probeTimer = null;
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

/**
 * Server-driven hold.
 *
 * The gesture-driven shield above can only release itself on `shiny:idle`, so
 * it is useless for an operation that keeps R continuously busy from start to
 * finish (typing's pre-run genome check hashes one assembly per reactive tick
 * and never yields an idle gap). Such an operation holds the UI explicitly
 * instead: it knows precisely when it begins and ends, so it says so.
 *
 * The hold deliberately does not use `.pt-shield`. A full-screen overlay wins
 * by z-index, which only orders elements inside the same stacking context - any
 * ancestor of a control that must stay live (Terminate) could trap it below the
 * overlay. Blocking through `pointer-events` instead reaches every descendant
 * of `body` regardless of stacking, and a single `pointer-events: auto` re-opens
 * exactly the controls marked `.pt-shield-passthrough`.
 */
function holdShield() {
  if (holdFailsafeTimer) clearTimeout(holdFailsafeTimer);
  document.documentElement.classList.add("pt-busy-hold");

  // The server should always release, but a crash mid-operation must not leave
  // the UI permanently dead.
  holdFailsafeTimer = setTimeout(function () {
    holdFailsafeTimer = null;
    // eslint-disable-next-line no-console
    console.warn("busy-shield: hold exceeded MAX_HOLD_MS, releasing");
    releaseHold();
  }, MAX_HOLD_MS);
}

function releaseHold() {
  if (holdFailsafeTimer) { clearTimeout(holdFailsafeTimer); holdFailsafeTimer = null; }
  document.documentElement.classList.remove("pt-busy-hold");
}

window.__ptHoldShield = holdShield;
window.__ptReleaseShieldHold = releaseHold;

document.addEventListener("pointerdown", function () {
  pointerDown = true;
}, true);

// Register gestures on `pointerup` so continuous drag operations (maps, sliders) only count when finished.
document.addEventListener("pointerup", function (event) {
  pointerDown = false;
  if (!isExemptFromShield(event.target)) lastGestureAt = Date.now();
}, true);

// Track key activation (Enter/Space) while allowing regular text input to stream unhindered.
document.addEventListener("keyup", function (event) {
  if (event.key !== "Enter" && event.key !== " ") return;
  if (!isExemptFromShield(event.target)) lastGestureAt = Date.now();
}, true);

// Suppress key triggers on currently focused UI elements while the shield is
// active. Pointer blocking alone would leave keyboard activation of an
// already-focused control working, which is the same queued-click problem by
// another route.
document.addEventListener("keydown", function (event) {
  var holding = document.documentElement.classList.contains("pt-busy-hold");
  if (!engaged && !holding) return;

  if (event.key === "Escape") {
    // A held operation is dismissed by its own control (Terminate), never by a
    // stray Escape - releasing here would unblock the UI mid-operation.
    if (!holding) release();
    return;
  }

  if (event.key === "Enter" || event.key === " ") {
    if (holding && !isShieldPassthrough(event.target)) {
      event.preventDefault();
      event.stopPropagation();
      return;
    }
    if (!holding) {
      event.preventDefault();
      event.stopPropagation();
    }
  }
}, true);

// Engage shield only when server work originates from a recent client gesture.
$(document).on("shiny:inputchanged", function () {
  if (pointerDown) return;
  if (Date.now() - lastGestureAt > GESTURE_WINDOW_MS) return;
  engage();
});

$(document).on("shiny:busy", function () {
  if (graceTimer) { clearTimeout(graceTimer); graceTimer = null; }
});

// Delay release during idle states to bridge momentary gaps between reactive cycles.
$(document).on("shiny:idle", function () {
  if (!engaged || graceTimer) return;
  graceTimer = setTimeout(function () {
    graceTimer = null;
    if (!serverIsBusy()) release();
  }, IDLE_GRACE_MS);
});

// A dead session can never send its own release, so drop both shields.
$(document).on("shiny:disconnected", function () {
  release();
  releaseHold();
});
