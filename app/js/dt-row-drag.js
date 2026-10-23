/**
 * Drag to select or unselect a run of rows in a DT grid using DT's own
 * multiple row selection (`selection = list(target = "row...")`).
 *
 * DT toggles the pressed row itself on mousedown; whatever state that leaves
 * it in is what the drag applies to every row it sweeps over, and rows the
 * drag leaves again go back to how they were. During the drag only the
 * `selected` class changes; on release the swept rows go to Shiny as one
 * {isolate, select} message, and the server applies them through
 * DT::selectRows(), which keeps DT's own selection state (and the
 * `_rows_selected` input) authoritative - including rows a filter is hiding.
 */

/**
 * @param {object} api DataTables API of the table.
 * @param {object} opts `inputId`: Shiny input receiving the swept rows;
 *   `isActive`: returns whether row selection is currently on.
 */
export function attachRowDrag(api, opts) {
  var body = api.table().body();
  var drag = null;

  // The display position of the body row `el` sits in, or -1.
  function rowOf(el) {
    var tr = el && el.closest ? el.closest("tr") : null;
    if (!tr || tr.parentNode !== body || tr.querySelector(".dataTables_empty")) return -1;
    return tr.sectionRowIndex;
  }

  function sweep(to) {
    if (to === drag.to) return;
    drag.to = to;
    var lo = Math.min(drag.from, to);
    var hi = Math.max(drag.from, to);
    for (var r = 0; r < body.rows.length; r++) {
      var on = r >= lo && r <= hi ? drag.select : drag.before[r];
      body.rows[r].classList.toggle("selected", on);
    }
  }

  // Bubble phase on the container, so DT's own mousedown (delegated on the
  // table) has already toggled the pressed row.
  api.table().container().addEventListener("mousedown", function (e) {
    if (e.button !== 0 || e.shiftKey || !opts.isActive()) return;
    var from = rowOf(e.target);
    if (from < 0) return;
    drag = {
      from: from,
      to: from,
      select: body.rows[from].classList.contains("selected"),
      before: [].map.call(body.rows, function (tr) {
        return tr.classList.contains("selected");
      }),
    };
  });

  document.addEventListener("mousemove", function (e) {
    if (!drag) return;
    var r = rowOf(document.elementFromPoint(e.clientX, e.clientY));
    if (r >= 0) sweep(r);
  });

  document.addEventListener("mouseup", function () {
    if (!drag) return;
    var d = drag;
    drag = null;
    if (d.to === d.from) return;
    var idx = api.rows({ search: "applied", order: "applied" }).indexes().toArray();
    var isolates = [];
    for (var r = Math.min(d.from, d.to); r <= Math.max(d.from, d.to); r++) {
      isolates.push(String(api.row(idx[r]).data()[0]));
    }
    Shiny.setInputValue(
      opts.inputId,
      { isolate: isolates, select: d.select },
      { priority: "event" },
    );
  });
}
