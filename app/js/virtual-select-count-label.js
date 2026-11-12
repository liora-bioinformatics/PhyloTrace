/**
 * Option renderer for virtual-select lists whose options and group headers
 * carry a count in `customData.count` (the Map's Show filter: isolates per
 * country and per continent). Draws the label with the count right-aligned.
 *
 * shinyWidgets resolves a `labelRenderer` given by name from `window`, so the
 * function is registered there. Options without a count keep their plain label.
 */

function countLabel(data) {
  var count = data.customData ? data.customData.count : undefined;
  // jsonlite sends R scalars as one-element arrays.
  if (Array.isArray(count)) count = count[0];
  if (count === undefined || count === null) return data.label;

  var countClass = Number(count) > 0 ? "pt-vs-count" : "pt-vs-count pt-vs-count-zero";
  return (
    '<span class="pt-vs-label">' + data.label + "</span>" +
    '<span class="' + countClass + '">' + count + "</span>"
  );
}

window.ptCountLabel = countLabel;
