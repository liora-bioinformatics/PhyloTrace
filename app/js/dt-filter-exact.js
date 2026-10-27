/**
 * Lets a single number typed into a DT number filter match exactly that value.
 *
 * DT's number and integer filters only understand a range, "from ... to"
 * (either side may be left out); a lone "44" is flagged as an error and the
 * table is left unfiltered. A bare number is rewritten to "44 ... 44" before
 * DT's own change handler reads it, so typing a value filters like the range
 * of that value does, and the slider follows.
 */

var BARE_NUMBER = /^\s*-?\d+(\.\d+)?\s*$/;

// Runs in the capture phase, ahead of the `change` handler DT binds to the input.
document.addEventListener(
  "change",
  function (event) {
    var input = event.target;
    if (!input || input.tagName !== "INPUT") return;
    var cell = input.closest("td[data-type]");
    var type = cell ? cell.getAttribute("data-type") : null;
    if (type !== "number" && type !== "integer") return;
    if (!BARE_NUMBER.test(input.value)) return;
    var value = input.value.trim();
    input.value = value + " ... " + value;
  },
  true,
);
