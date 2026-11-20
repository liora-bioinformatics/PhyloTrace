/**
 * Delayed, minimum-duration busy state for a container of Shiny outputs.
 *
 * An element with class `pt-delayed-busy` gets `is-busy` once an output inside
 * it has been recalculating for SHOW_AFTER_MS, and keeps it for at least
 * MIN_VISIBLE_MS. A fast update therefore never shows a busy indicator at all,
 * and a slow one never flashes it for a few frames. Styling of the indicator
 * is up to the container (see `.landing-db_busy` in main.scss).
 *
 * Keyed on Shiny's `shiny:recalculating` / `shiny:recalculated` events, which
 * fire as soon as the server starts and finishes an output, rather than on the
 * `recalculating` class, which Shiny only applies when the values arrive.
 */
const SHOW_AFTER_MS = 150;
const MIN_VISIBLE_MS = 450;

const states = new WeakMap();

function hostState(host) {
  let state = states.get(host);
  if (!state) {
    state = {
      running: 0, showTimer: null, hideTimer: null, shownAt: null,
    };
    states.set(host, state);
  }
  return state;
}

function hostOf(event) {
  const { target } = event;
  return target && target.closest ? target.closest('.pt-delayed-busy') : null;
}

$(document).on('shiny:recalculating', (event) => {
  const host = hostOf(event);
  if (!host) return;
  const state = hostState(host);
  state.running += 1;
  clearTimeout(state.hideTimer);
  state.hideTimer = null;
  if (state.shownAt !== null || state.showTimer !== null) return;
  state.showTimer = setTimeout(() => {
    state.showTimer = null;
    state.shownAt = Date.now();
    host.classList.add('is-busy');
  }, SHOW_AFTER_MS);
});

$(document).on('shiny:recalculated', (event) => {
  const host = hostOf(event);
  if (!host) return;
  const state = hostState(host);
  state.running = Math.max(state.running - 1, 0);
  if (state.running > 0) return;

  if (state.showTimer !== null) {
    clearTimeout(state.showTimer);
    state.showTimer = null;
    return;
  }
  if (state.shownAt === null) return;

  const remaining = Math.max(MIN_VISIBLE_MS - (Date.now() - state.shownAt), 0);
  state.hideTimer = setTimeout(() => {
    state.hideTimer = null;
    state.shownAt = null;
    host.classList.remove('is-busy');
  }, remaining);
});
