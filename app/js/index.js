document.addEventListener("input", function (event) {
  if ((event.target.tagName === "INPUT" && event.target.type === "text") || event.target.classList.contains("handsontableInput")) {
    const regex = /^[a-zA-Z0-9_-]*$/;
    if (!regex.test(event.target.value)) {
      event.target.value = event.target.value.replace(/[^a-zA-Z0-9_-]/g, "");
    }
  }
});

$(document).one("shiny:idle", function () {
  setTimeout(function () {
    var overlay = document.querySelector(".waiter-overlay");
    if (!overlay) return;
    overlay.style.transition = "opacity 0.4s ease";
    overlay.style.opacity = "0";
    setTimeout(function () { overlay.remove(); }, 400);
  }, 1500);
});
