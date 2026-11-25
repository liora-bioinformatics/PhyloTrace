/**
 * Plot reveal: one loading state per plot stage, from Generate until the plot
 * is actually on screen.
 *
 * Shiny's busy state ends when the server is done, but every plot engine still
 * has work left after that: the PLOT_SETTLE_MS debounce leaves an idle gap
 * between Generate and the render, an image has to decode, vis.js has to paint
 * the network, and Leaflet has to build the map and fetch its tiles. The
 * gesture shield (busy-shield.js) releases on the first idle, so the UI used to
 * unblock before the plot could be seen. Here the stage's `.is-loading` overlay
 * and a UI hold share one pending entry, and both end only once that engine's
 * plot has painted.
 *
 * A stage opts in with two data attributes (set in each engine's plot_area):
 *   data-pt-output  the id of the Shiny output that draws the plot
 *   data-pt-reveal  what counts as painted once that output has its value:
 *     "image"   the output's <img> has loaded and decoded the new picture
 *     "leaflet" the map exists and its tile layers have finished loading
 *     "drawn"   the widget reports its own first draw via __ptPlotDrawn()
 * The Generate button names its stage in `data-pt-stage`, so a click on it
 * (real, or synthesized by click_generate()) starts the entry.
 *
 * Entry points for the server and for widget code:
 *   window.__ptPlotLoading(stageId, true)   start, before the stage may exist
 *   window.__ptPlotLoading(stageId, false)  end now (a Generate that bailed)
 *   window.__ptPlotDrawn(el)                a widget inside a stage has drawn
 */

// A Generate that produces no value at all (an early return the server did not
// report) leaves the server idle; after this long idle the entry is dropped.
// Comfortably past PLOT_SETTLE_MS, the longest idle gap of a healthy Generate.
var IDLE_BAIL_MS = 3000;
// Once the value has arrived, how long painting may take at most.
var PAINT_MAX_MS = 15000;
// How long Leaflet's tiles may take before the map counts as shown anyway (an
// offline machine never finishes loading them).
var TILE_WAIT_MS = 4000;
// How long a stage started before it was mounted (a tab reopened from the
// Analysis Dashboard) is looked for.
var MOUNT_WAIT_MS = 10000;
// Bound on the UI hold itself; the entry normally ends far sooner.
var HOLD_MAX_MS = 10 * 60 * 1000;

// Pending entries by stage id.
var pending = {};
var idleTimer = null;

function holdKey(stageId) {
  return "plot:" + stageId;
}

function serverIsBusy() {
  return document.documentElement.classList.contains("shiny-busy");
}

// Runs `fn` once the browser has painted the current frame.
function afterPaint(fn) {
  requestAnimationFrame(function () {
    requestAnimationFrame(fn);
  });
}

// Puts the overlay on a mounted stage and reads what it reveals by.
function attach(entry) {
  var stage = document.getElementById(entry.stageId);
  if (!stage) return false;
  stage.classList.add("is-loading");
  var prompt = stage.querySelector(".viz-plot-prompt");
  if (prompt) prompt.style.display = "none";
  entry.output = stage.getAttribute("data-pt-output");
  entry.reveal = stage.getAttribute("data-pt-reveal") || "image";
  return true;
}

// Watches for a stage that was started before it was mounted. A mutation
// callback runs before the next paint, so the stage never shows its "press
// Generate" prompt first.
function waitForMount(entry) {
  if (entry.mountObserver) return;
  var observer = new MutationObserver(function () {
    if (pending[entry.stageId] !== entry || attach(entry)) stopWaiting(entry);
  });
  observer.observe(document.body, { childList: true, subtree: true });
  entry.mountObserver = observer;
  entry.mountTimer = setTimeout(function () { stopWaiting(entry); }, MOUNT_WAIT_MS);
}

function stopWaiting(entry) {
  if (entry.mountObserver) {
    entry.mountObserver.disconnect();
    entry.mountObserver = null;
  }
  clearTimeout(entry.mountTimer);
}

function armIdleBail() {
  if (idleTimer) return;
  idleTimer = setTimeout(function () {
    idleTimer = null;
    if (serverIsBusy()) return;
    Object.keys(pending).forEach(function (id) {
      if (!pending[id].gotValue) finish(id);
    });
  }, IDLE_BAIL_MS);
}

function disarmIdleBail() {
  if (idleTimer) {
    clearTimeout(idleTimer);
    idleTimer = null;
  }
}

function start(stageId) {
  var entry = pending[stageId];
  if (!entry) {
    entry = { stageId: stageId, gotValue: false };
    pending[stageId] = entry;
  } else {
    // A second Generate on a stage still loading starts its wait over.
    entry.gotValue = false;
    clearTimeout(entry.paintTimer);
  }
  if (window.__ptHoldShield) window.__ptHoldShield(HOLD_MAX_MS, holdKey(stageId));
  if (!attach(entry)) waitForMount(entry);
  // Restarted rather than left running, so an idle wait that began for an
  // earlier stage cannot drop this one before it has had its full allowance.
  disarmIdleBail();
  if (!serverIsBusy()) armIdleBail();
}

function finish(stageId) {
  var entry = pending[stageId];
  if (entry) {
    stopWaiting(entry);
    clearTimeout(entry.paintTimer);
    delete pending[stageId];
  }
  var stage = document.getElementById(stageId);
  if (stage) stage.classList.remove("is-loading");
  if (window.__ptReleaseShieldHold) window.__ptReleaseShieldHold(holdKey(stageId));
  if (Object.keys(pending).length === 0) disarmIdleBail();
}

function finishAfterPaint(entry) {
  afterPaint(function () {
    if (pending[entry.stageId] === entry) finish(entry.stageId);
  });
}

// The output's <img> once it carries the new value's picture. Shiny fires
// `shiny:value` before the binding writes the src, so this waits a frame or
// two for the swap.
function awaitImage(entry, src) {
  var tries = 0;
  (function look() {
    if (pending[entry.stageId] !== entry) return;
    var out = document.getElementById(entry.output);
    var img = out && out.querySelector("img");
    if (img && (!src || img.getAttribute("src") === src)) {
      if (img.decode) {
        img.decode().then(
          function () { finishAfterPaint(entry); },
          function () { finishAfterPaint(entry); }
        );
      } else {
        finishAfterPaint(entry);
      }
      return;
    }
    tries += 1;
    if (tries > 120) {
      finishAfterPaint(entry);
      return;
    }
    requestAnimationFrame(look);
  })();
}

// The map once it is built and every tile layer on it has loaded.
function awaitLeaflet(entry) {
  var tries = 0;
  (function look() {
    if (pending[entry.stageId] !== entry) return;
    var widget = window.HTMLWidgets && HTMLWidgets.find("#" + entry.output);
    var map = widget && widget.getMap && widget.getMap();
    if (!map) {
      tries += 1;
      if (tries > 120) finishAfterPaint(entry);
      else requestAnimationFrame(look);
      return;
    }
    var loading = [];
    map.eachLayer(function (layer) {
      if (typeof layer.isLoading === "function" && layer.isLoading()) {
        loading.push(layer);
      }
    });
    if (!loading.length) {
      finishAfterPaint(entry);
      return;
    }
    var left = loading.length;
    var cap = setTimeout(function () { finishAfterPaint(entry); }, TILE_WAIT_MS);
    loading.forEach(function (layer) {
      layer.once("load", function () {
        left -= 1;
        if (left === 0) {
          clearTimeout(cap);
          finishAfterPaint(entry);
        }
      });
    });
  })();
}

function entryFor(outputId) {
  var ids = Object.keys(pending);
  for (var i = 0; i < ids.length; i++) {
    if (pending[ids[i]].output === outputId) return pending[ids[i]];
  }
  return null;
}

$(document).on("shiny:value", function (event) {
  var entry = entryFor(event.target && event.target.id);
  if (!entry || entry.gotValue) return;
  entry.gotValue = true;
  entry.paintTimer = setTimeout(function () {
    if (pending[entry.stageId] === entry) finish(entry.stageId);
  }, PAINT_MAX_MS);
  // Leaflet's renderValue runs right after this event, so look a beat later.
  setTimeout(function () {
    if (entry.reveal === "image") {
      awaitImage(entry, event.value && event.value.src);
    } else if (entry.reveal === "leaflet") {
      awaitLeaflet(entry);
    }
    // "drawn" waits for __ptPlotDrawn().
  }, 0);
});

// A failed render paints its error message instead; that is the end too. A
// silent error (a `req()` that failed, empty message) paints nothing: Shiny
// only clears the output. Every engine's render hits one while a reopened
// plot's inputs are still arriving, before the plot it is waiting for exists,
// so it must not end the wait. A Generate that only ever fails silently is
// ended by the idle bail instead.
$(document).on("shiny:error", function (event) {
  var entry = entryFor(event.target && event.target.id);
  if (!entry || !event.error || event.error.message === "") return;
  finishAfterPaint(entry);
});

$(document).on("shiny:busy", disarmIdleBail);
$(document).on("shiny:idle", function () {
  var waiting = Object.keys(pending).some(function (id) {
    return !pending[id].gotValue;
  });
  if (waiting) armIdleBail();
});

$(document).on("shiny:disconnected", function () {
  Object.keys(pending).forEach(finish);
});

// Generate (and click_generate()'s synthetic click) starts its stage. Capture
// phase, so a handler further down that stops propagation cannot hide it.
document.addEventListener("click", function (event) {
  var target = event.target;
  var button = target && target.closest && target.closest("[data-pt-stage]");
  if (!button || button.disabled) return;
  start(button.getAttribute("data-pt-stage"));
}, true);

window.__ptPlotLoading = function (stageId, on) {
  if (on) start(stageId);
  else finish(stageId);
};

// Called from a widget's own draw hook with any element inside its stage.
window.__ptPlotDrawn = function (el) {
  var stage = el && el.closest && el.closest(".viz-plot-stage");
  var entry = stage && pending[stage.id];
  // A draw before the new value arrived is the previous plot redrawing.
  if (!entry || !entry.gotValue || entry.reveal !== "drawn") return;
  finishAfterPaint(entry);
};
