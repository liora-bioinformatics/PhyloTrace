/**
 * Browser-side playback for the time animations of the Epi curve and the Map.
 *
 * Pressing Play makes the server prepare the whole run once (the Epi curve's
 * images, the Map's per-frame layer states; see app/logic/viz_anim.R and the
 * two view modules) and send it here. The run then plays on the browser's own
 * clock, so no frame waits for the server, and reports back to the module's
 * `anim_event` input when it ends: played out (`done`) or paused (`stopped`).
 *
 * Epi curve: the run is drawn over the plot image, in a layer outside the
 * Shiny output (which reuses the first <img> inside it). A "wipe" run uncovers
 * the fully revealed curve over its first frame from left to right, a
 * continuous sweep through the bin edges; a "frames" run is a flipbook of
 * pre-rendered images. A paused run's layer stays up until the plot below has
 * re-rendered to the same window, so the hand-over shows no jump.
 *
 * Map: the layers already on the map are stepped in place: markers join and
 * leave their cluster, region fills ease between counts (a CSS transition, see
 * main.scss), heat intensities blend, minicharts re-slice, and the legend and
 * date label follow the frame. The basemap and the view are never touched.
 */

// A preparation shorter than this shows no notice, so the quick ones don't flash.
var NOTICE_DELAY_MS = 250;
// How long a paused Epi run's layer waits for the re-rendered plot at most.
var RELEASE_TIMEOUT_MS = 15000;
// Share of a Map frame over which the heat blends into the next frame's.
var HEAT_BLEND = 0.85;

// Returns `x` as an array: Shiny's JSON unboxes one-element vectors.
function asArray(x) {
  if (x === null || x === undefined) return [];
  return Array.isArray(x) ? x : [x];
}

// Reports a run's end to the module's `anim_event` input.
function report(inputId, run, values) {
  Shiny.setInputValue(inputId, Object.assign({ run: run }, values), { priority: "event" });
}

// Calls `fn` on every animation frame until it returns false.
function loop(fn) {
  var handle = { id: null, stopped: false };
  function step(now) {
    if (handle.stopped) return;
    if (fn(now) === false) return;
    handle.id = requestAnimationFrame(step);
  }
  handle.id = requestAnimationFrame(step);
  return handle;
}

// Stops a loop() handle.
function stopLoop(handle) {
  if (!handle) return;
  handle.stopped = true;
  if (handle.id !== null) cancelAnimationFrame(handle.id);
}

// ---- Epi curve -------------------------------------------------------------

// Per plot output id: the run on screen (or being prepared).
var epiRuns = {};

// Shows or clears the "Preparing animation" notice on the plot stage, reusing
// its loading overlay. `text` null clears it.
function stageNotice(run, text) {
  var stage = document.getElementById(run.stage);
  if (!stage) return;
  var label = stage.querySelector(".viz-loading_text");
  clearTimeout(run.noticeTimer);
  if (text === null) {
    if (run.noticeShown) {
      stage.classList.remove("is-loading");
      if (label && run.noticeText !== undefined) label.textContent = run.noticeText;
    }
    run.noticeShown = false;
    return;
  }
  var show = function () {
    if (!run.noticeShown && label) run.noticeText = label.textContent;
    run.noticeShown = true;
    if (label) label.textContent = text;
    stage.classList.add("is-loading");
  };
  if (run.noticeShown) show();
  else run.noticeTimer = setTimeout(show, NOTICE_DELAY_MS);
}

// Loads and decodes every image of a run, so no frame waits to paint.
function loadImages(urls) {
  return Promise.all(
    urls.map(function (url) {
      var img = new Image();
      img.decoding = "async";
      img.src = url;
      return img.decode().then(function () {
        return img;
      });
    })
  );
}

// Keeps the run's layer exactly over the plot image, wherever zoom or pan
// has put it.
function alignEpi(run) {
  var out = document.getElementById(run.output);
  var img = out && out.querySelector("img");
  if (!img || !run.layer) return;
  var view = run.viewport.getBoundingClientRect();
  var box = img.getBoundingClientRect();
  var style = run.layer.style;
  style.left = box.left - view.left + run.viewport.scrollLeft + "px";
  style.top = box.top - view.top + run.viewport.scrollTop + "px";
  style.width = box.width + "px";
  style.height = box.height + "px";
}

// Draws a run at position `t`, in bins from its start.
function drawEpi(run, t) {
  if (run.kind === "wipe") {
    var edges = run.edges;
    var i = Math.min(Math.floor(t), edges.length - 2);
    var x = edges[i] + (edges[i + 1] - edges[i]) * (t - i);
    run.top.style.clipPath = "inset(0 " + (100 - x * 100).toFixed(3) + "% 0 0)";
    return;
  }
  var k = Math.min(Math.floor(t), run.images.length - 1);
  if (k !== run.shown) {
    run.layer.replaceChildren(run.images[k]);
    run.shown = k;
  }
}

// Takes a run's layer off the plot and forgets the run.
function dropEpi(run) {
  stopLoop(run.loop);
  stageNotice(run, null);
  clearTimeout(run.releaseTimer);
  if (run.onValue) $(document).off("shiny:value", run.onValue);
  if (run.layer) run.layer.remove();
  if (epiRuns[run.output] === run) delete epiRuns[run.output];
}

// Starts a prepared run once its images are decoded.
function playEpi(run, msg) {
  run.kind = msg.kind;
  run.stepMs = msg.step_ms;
  run.edges = asArray(msg.edges);
  var urls = asArray(msg.urls);
  // Bins the run reveals: one per wipe segment, one per flipbook image.
  run.bins = run.kind === "wipe" ? run.edges.length - 1 : urls.length;
  stageNotice(run, "Loading animation …");
  loadImages(urls).then(
    function (images) {
      if (epiRuns[run.output] !== run || run.ended) return;
      var out = document.getElementById(run.output);
      run.viewport = out && out.closest(".viz-plot-scroll");
      if (!run.viewport) {
        dropEpi(run);
        return;
      }
      run.layer = document.createElement("div");
      run.layer.className = "viz-anim-layer";
      if (run.kind === "wipe") {
        images[0].className = "viz-anim-base";
        images[1].className = "viz-anim-top";
        run.top = images[1];
        run.layer.append(images[0], images[1]);
      } else {
        run.images = images;
        run.shown = -1;
      }
      drawEpi(run, 0);
      run.viewport.appendChild(run.layer);
      alignEpi(run);
      stageNotice(run, null);
      var start = null;
      run.loop = loop(function (now) {
        alignEpi(run);
        if (run.held) return true;
        if (start === null) start = now;
        run.t = Math.min((now - start) / run.stepMs, run.bins);
        drawEpi(run, run.t);
        if (run.t >= run.bins) {
          run.ended = true;
          report(run.input, run.run, { type: "done", bins: run.bins });
          dropEpi(run);
          return false;
        }
        return true;
      });
    },
    function () {
      report(run.input, run.run, { type: "done", bins: run.bins });
      dropEpi(run);
    }
  );
}

// Holds a playing run on the last bin it has fully revealed and reports it.
function pauseEpi(run) {
  if (run.ended) return;
  run.ended = true;
  run.held = true;
  var shown = Math.max(1, Math.min(Math.floor(run.t || 0), run.bins));
  // A flipbook's frame k shows k + 1 bins.
  if (run.kind !== "wipe") shown = Math.min((run.shown || 0) + 1, run.bins);
  if (run.layer) drawEpi(run, run.kind === "wipe" ? shown : shown - 1);
  report(run.input, run.run, { type: "stopped", bins: shown });
}

// Takes a held run's layer down: now, or once the plot below has re-rendered
// and its new image is decoded.
function releaseEpi(run, wait) {
  if (!wait || !run.layer) {
    dropEpi(run);
    return;
  }
  run.onValue = function (event) {
    if (event.target.id !== run.output) return;
    $(document).off("shiny:value", run.onValue);
    run.onValue = null;
    // The output sets its image's new source once this event has run.
    setTimeout(function () {
      var img = document.querySelector("#" + CSS.escape(run.output) + " img");
      var done = function () {
        dropEpi(run);
      };
      if (img && img.decode) img.decode().then(done, done);
      else done();
    }, 0);
  };
  $(document).on("shiny:value", run.onValue);
  run.releaseTimer = setTimeout(function () {
    dropEpi(run);
  }, RELEASE_TIMEOUT_MS);
}

Shiny.addCustomMessageHandler("pt-epi-anim", function (msg) {
  var run = epiRuns[msg.output];
  if (msg.cmd === "prepare") {
    if (run) dropEpi(run);
    run = { output: msg.output, stage: msg.stage, input: msg.input, run: msg.run };
    epiRuns[msg.output] = run;
    stageNotice(run, "Preparing animation …");
    return;
  }
  if (!run || run.run !== msg.run) return;
  if (msg.cmd === "progress") {
    stageNotice(run, "Preparing animation … " + Math.round(msg.value * 100) + "%");
  } else if (msg.cmd === "play") {
    playEpi(run, msg);
  } else if (msg.cmd === "pause") {
    // Still loading: nothing was shown, so there is nothing to hold.
    if (!run.layer) {
      run.ended = true;
      report(run.input, run.run, { type: "done", bins: run.bins });
      dropEpi(run);
    } else {
      pauseEpi(run);
    }
  } else if (msg.cmd === "release") {
    releaseEpi(run, msg.wait === true);
  } else if (msg.cmd === "abort") {
    run.ended = true;
    dropEpi(run);
  }
});

// ---- Map -------------------------------------------------------------------

// Per map output id: the run playing on it.
var mapRuns = {};

// The Leaflet map behind a map output id, or null.
function leafletMap(id) {
  var widget = window.HTMLWidgets && window.HTMLWidgets.find("#" + CSS.escape(id));
  return widget && widget.getMap ? widget.getMap() : null;
}

// Markers: shows exactly those whose first frame is at or before frame k,
// adding and removing only the difference to what is shown now.
function markersTo(run, k) {
  var st = run.markers;
  if (!st.cluster) return;
  var add = [];
  var remove = [];
  for (var i = 0; i < st.layers.length; i++) {
    var layer = st.layers[i];
    if (!layer) continue;
    var want = st.appear[i] <= k + 1;
    var have = st.appear[i] <= st.at + 1;
    if (want && !have) add.push(layer);
    else if (!want && have) remove.push(layer);
  }
  if (remove.length) st.cluster.removeLayers(remove);
  if (add.length) {
    st.cluster.addLayers(add);
    add.forEach(function (marker) {
      // Only markers drawn on their own have a path; clustered ones show in
      // their cluster's count.
      var path = marker._path;
      if (!path) return;
      path.classList.remove("map-anim-in");
      void path.getBoundingClientRect();
      path.classList.add("map-anim-in");
    });
  }
  st.at = k;
  var fills = st.fills && asArray(st.fills[k]);
  if (fills) {
    for (var j = 0; j < st.layers.length; j++) {
      if (st.layers[j] && fills[j]) st.layers[j].setStyle({ fillColor: fills[j] });
    }
  }
}

// Regions: restyles every region to frame k, its fill, label and whether the
// label is pinned.
function regionsTo(run, k) {
  var st = run.regions;
  var fills = asArray(st.fills[k]);
  var labels = asArray(st.labels[k]);
  var pinned = asArray(st.pinned[k]);
  for (var i = 0; i < st.layers.length; i++) {
    var layer = st.layers[i];
    if (!layer) continue;
    layer.setStyle({ fillColor: fills[i] });
    var tooltip = layer.getTooltip && layer.getTooltip();
    if (!tooltip) continue;
    if (!!tooltip.options.permanent !== !!pinned[i]) {
      var options = Object.assign({}, tooltip.options, { permanent: !!pinned[i] });
      layer.unbindTooltip();
      layer.bindTooltip(labels[i], options);
      if (pinned[i]) layer.openTooltip();
    } else if (st.lastLabels[i] !== labels[i]) {
      layer.setTooltipContent(labels[i]);
    }
    st.lastLabels[i] = labels[i];
  }
}

// Heat: draws the blend `f` (0 to 1) of frame `from` into frame `to`.
function heatBlend(run, from, to, f) {
  var st = run.heat;
  if (!st.layer) return;
  var a = asArray(st.counts[from]);
  var b = asArray(st.counts[to]);
  var points = [];
  for (var i = 0; i < st.lat.length; i++) {
    var v = a[i] + (b[i] - a[i]) * f;
    // leaflet.heat draws a zero-intensity point at its minimum opacity.
    if (v > 1e-3) points.push([st.lat[i], st.lng[i], v]);
  }
  var max = st.max[from] + (st.max[to] - st.max[from]) * f;
  // setOptions() would also rebuild the gradient and brush on every frame;
  // the layer caps each point at options.max and its renderer scales by its own.
  st.layer.options.max = max;
  if (st.layer._heat) st.layer._heat.max(max);
  st.layer.setLatLngs(points);
}

// Swaps in frame k's legend where it differs from the one shown.
function legendTo(run, k) {
  var legends = run.legends;
  if (!legends) return;
  var idx = asArray(legends.idx)[k];
  if (idx === legends.at) return;
  legends.at = idx;
  var methods = window.LeafletWidget.methods;
  if (idx === null || idx === undefined) {
    methods.removeControl.call(run.map, "map-legend");
  } else {
    methods.addLegend.apply(run.map, asArray(legends.items)[idx - 1]);
  }
  var el = run.map.getContainer();
  if (el.__ptLegendFit) el.__ptLegendFit();
}

// Shows frame k: the mode's layer, the legend and the date labels.
function mapTo(run, k) {
  if (run.kind === "markers") markersTo(run, k);
  else if (run.kind === "regions") regionsTo(run, k);
  else if (run.kind === "charts") {
    window.LeafletWidget.methods.updateMinicharts.apply(run.map, asArray(run.charts[k]));
  }
  legendTo(run, k);
  var label = run.labels[k];
  var onMap = run.map.getContainer().querySelector(".map-date-label");
  if (onMap) onMap.textContent = label;
  var side = document.getElementById(run.label);
  if (side) side.textContent = label;
  run.k = k;
}

// Stops a run; `restore` first puts the map back on its last frame, the map
// as drawn.
function stopMap(run, restore) {
  stopLoop(run.loop);
  if (restore && run.k !== run.n - 1) {
    try {
      mapTo(run, run.n - 1);
      if (run.kind === "heat") heatBlend(run, run.n - 1, run.n - 1, 1);
    } catch (e) {
      // The map was re-rendered under the run; the new render stands.
    }
  }
  if (mapRuns[run.id] === run) delete mapRuns[run.id];
}

// Finds the drawn layers a run steps, by the ids the server gave them.
function bindMapLayers(run, msg) {
  var lm = run.map.layerManager;
  if (run.kind === "markers") {
    var cluster = lm.getLayer("cluster", msg.markers.cluster);
    var store = cluster && cluster.clusterLayerStore ? cluster.clusterLayerStore._layers : {};
    var ids = asArray(msg.markers.ids);
    run.markers = {
      cluster: cluster,
      layers: ids.map(function (id) {
        return store[id] || null;
      }),
      appear: asArray(msg.markers.appear),
      fills: msg.markers.fills ? asArray(msg.markers.fills) : null,
      // The map shows the last frame when the run starts.
      at: run.n - 1,
    };
  } else if (run.kind === "regions") {
    run.regions = {
      layers: asArray(msg.regions.ids).map(function (id) {
        return lm.getLayer("shape", id) || null;
      }),
      fills: asArray(msg.regions.fills),
      labels: asArray(msg.regions.labels),
      pinned: asArray(msg.regions.pinned),
      lastLabels: [],
    };
  } else if (run.kind === "heat") {
    run.heat = {
      layer: lm.getLayer("heatmap", msg.heat.id),
      lat: asArray(msg.heat.lat),
      lng: asArray(msg.heat.lng),
      counts: asArray(msg.heat.counts),
      max: asArray(msg.heat.max),
    };
  } else if (run.kind === "charts") {
    run.charts = asArray(msg.charts);
  }
}

// Plays a run on the map: frame k shows for step_ms, the heat blends into it.
function playMap(msg) {
  var old = mapRuns[msg.map];
  if (old) stopMap(old, false);
  var map = leafletMap(msg.map);
  if (!map) {
    report(msg.input, msg.run, { type: "done", frame: msg.n });
    return;
  }
  var run = {
    id: msg.map,
    input: msg.input,
    label: msg.label,
    run: msg.run,
    map: map,
    n: msg.n,
    stepMs: msg.step_ms,
    kind: msg.kind,
    labels: asArray(msg.labels),
    legends: msg.legends ? { idx: msg.legends.idx, items: msg.legends.items, at: undefined } : null,
    k: msg.n - 1,
  };
  bindMapLayers(run, msg);
  mapRuns[msg.map] = run;
  var start = null;
  var heatFrom = run.n - 1;
  run.loop = loop(function (now) {
    if (start === null) start = now;
    var t = (now - start) / run.stepMs;
    if (t >= run.n) {
      if (run.kind === "heat") heatBlend(run, run.n - 1, run.n - 1, 1);
      stopMap(run, false);
      report(run.input, run.run, { type: "done", frame: run.n });
      return false;
    }
    var k = Math.floor(t);
    if (k !== run.k) {
      heatFrom = run.k;
      mapTo(run, k);
    }
    if (run.kind === "heat") {
      var f = Math.min(1, (t - k) / HEAT_BLEND);
      // Eased, so a place swells into its new intensity rather than ramping.
      heatBlend(run, heatFrom, k, f * f * (3 - 2 * f));
    }
    return true;
  });
}

Shiny.addCustomMessageHandler("pt-map-anim", function (msg) {
  if (msg.cmd === "play") {
    playMap(msg);
    return;
  }
  var run = mapRuns[msg.map];
  if (!run || run.run !== msg.run) return;
  if (msg.cmd === "pause") {
    stopMap(run, false);
    if (run.kind === "heat") heatBlend(run, run.k, run.k, 1);
    report(run.input, run.run, { type: "stopped", frame: run.k + 1 });
  } else if (msg.cmd === "abort") {
    stopMap(run, true);
  }
});
