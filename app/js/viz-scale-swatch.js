/**
 * Mirrors a color-scale picker's selected <option> style onto its own toggle
 * button, so the palette swatch set by app/logic/viz_helpers.R's
 * scale_select() (a `style` choicesOpt, which bootstrap-select applies to the
 * rendered <a class="dropdown-item">) survives closing the dropdown instead
 * of collapsing back to plain text. bootstrap-select has no built-in path for
 * an option's style to reach the button that way.
 */
$(document).on("rendered.bs.select", ".viz-scale-select select", function () {
  var select = this;
  var button = $(select).closest(".bootstrap-select").children(".dropdown-toggle")[0];
  if (!button) return;
  var option = select.options[select.selectedIndex];
  button.setAttribute("style", (option && option.getAttribute("style")) || "");
});
