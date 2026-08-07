/**
 * Blocks the top-level tab navigation for the brief window between clicking
 * "Start Analysis" and the Results table's first paint.
 *
 * Engaged synchronously on click, client-side, rather than waiting on a
 * server round trip to confirm a run actually started - closes the window
 * completely, including the run's own pre-flight genome check (which can
 * itself take a moment for a large selection). The Results table's *first*
 * render is the one that behaves unreliably if the "Add Isolates" tab is
 * switched away before it lands client-side - once that first paint has
 * happened, later live updates (dataTableProxy / replaceData, firing every
 * poll tick for the rest of the run) keep working correctly even while the
 * tab stays hidden. So rather than locking the whole UI for a run's entire
 * duration (which busy-shield.js deliberately does not do - see typing.R's
 * `pt-no-lock` on Start/Terminate), this narrows the block to just that one
 * short window, and only over the tab bar - every other control (Terminate
 * included) stays live.
 *
 * Released either by the Results table's own initComplete callback (see
 * typing.R's header_tooltips) once a run actually reaches that first paint,
 * or by typing.R directly (runjs) on every path that stops short of it - an
 * empty queue after filtering, or the pre-run genome check being terminated.
 * FAILSAFE_MS is the last-resort backstop against any path that misses both.
 */
var FAILSAFE_MS = 8000;
var navEl = null;
var failsafeTimer = null;

function release() {
  if (!navEl) return;
  clearTimeout(failsafeTimer);
  navEl.classList.remove("pt-nav-locked");
  navEl = null;
}

document.addEventListener("click", function (event) {
  if (!event.target.closest(".pt-typing-start")) return;
  navEl = document.getElementById("tabs");
  if (!navEl) return;
  navEl.classList.add("pt-nav-locked");
  clearTimeout(failsafeTimer);
  failsafeTimer = setTimeout(release, FAILSAFE_MS);
});

// Called directly (no server round trip needed) from the results table's own
// initComplete callback, or via runjs() from typing.R's early-exit paths.
window.__ptReleaseTypingNavLock = release;
