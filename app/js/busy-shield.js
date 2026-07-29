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
    (el.tagName === "INPUT" && el.type === "number") // Stepper arrows
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

// Suppress key triggers on currently focused UI elements while the shield is active.
document.addEventListener("keydown", function (event) {
  if (!engaged) return;
  if (event.key === "Escape") {
    release(); // Manual override
    return;
  }
  if (event.key === "Enter" || event.key === " ") {
    event.preventDefault();
    event.stopPropagation();
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

$(document).on("shiny:disconnected", release);
