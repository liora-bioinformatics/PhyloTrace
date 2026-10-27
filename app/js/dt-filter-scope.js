/**
 * Scopes DT's column filter functions to their own table.
 *
 * For every column of a client-side table with `filter = "top"`, DT pushes one
 * function onto DataTables' global `ext.search`, and DataTables runs every
 * function on that list for every row of every client-side table at every
 * draw - none is ever removed, not even when its table is re-rendered. Each
 * call first asks whether it belongs to the table being drawn, by building a
 * DataTables API object (`table.settings()[0] !== settings`), then whether its
 * column filter is set at all, and keeps the row when either answer is no. On
 * an editable grid of ~1,700 rows and 24 columns that was ~75 ms of every draw
 * - every edit - with no filter set.
 *
 * Each such function is wrapped as DT pushes it, with both questions answered
 * cheaply first: the table it was pushed for (the one whose DataTables
 * initialisation ran last, which DT's widget does just before pushing its
 * filters) and the filter cell of its column, whose `filter` data DT sets
 * while the filter is in use. Only a set filter on its own table runs DT's
 * function, so every result is the same as before. The functions of a table
 * no longer on the page - one a re-render replaced - are dropped whenever the
 * next table is set up.
 */

// DT's filter functions, told apart from anything else on `ext.search` by
// the check they start with.
var SIGNATURE = "table.settings()[0] !== settings";

var owner = null;
var pushed = 0;

function scope(fn) {
  var settings = owner;
  var k = pushed++;
  var cell;
  var active = true;
  var scoped = function (s, data, index, values, counter) {
    if (s !== settings) return true;
    // A filter pass calls each function for every row in turn, `counter`
    // counting from 0: whether the filter is set is read once per pass.
    if (counter > 0 && !active) return true;
    // DT pushes one function per cell of the filter row (the header's last
    // row), in column order. Read from DataTables' header layout, which
    // keeps the cells of hidden columns too. Filters in the footer, or
    // columns ColReorder has moved, leave every call to DT's own function.
    if (cell === undefined) {
      var layout = settings.aoHeader[settings.aoHeader.length - 1];
      cell = layout && layout[k] ? layout[k].cell : null;
      if (!cell || cell.tagName !== "TD" || settings._colReorder) cell = null;
    }
    active = !cell || !!$.data(cell, "filter");
    if (!active) return true;
    return fn.apply(this, arguments);
  };
  scoped.ptOwner = settings;
  return scoped;
}

// Never inside a filter pass: called as a table is set up.
function prune(search) {
  for (var i = search.length - 1; i >= 0; i--) {
    var table = search[i].ptOwner ? search[i].ptOwner.nTable : null;
    if (table && !document.body.contains(table)) search.splice(i, 1);
  }
}

function install() {
  var search = $.fn.dataTable.ext.search;
  if (search.ptScoped) return;
  var push = search.push;
  search.push = function () {
    var fns = [].map.call(arguments, function (fn) {
      return owner && typeof fn === "function" && String(fn).indexOf(SIGNATURE) !== -1
        ? scope(fn)
        : fn;
    });
    return push.apply(this, fns);
  };
  search.ptScoped = true;
}

$(document).on("preInit.dt", function (e, settings) {
  if (!$.fn.dataTable) return;
  install();
  prune($.fn.dataTable.ext.search);
  owner = settings;
  pushed = 0;
});
