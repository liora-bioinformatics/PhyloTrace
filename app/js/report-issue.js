// Issue reporting: a Report button on every error notification, plus the
// navbar #report-issue button. Both screenshot the page (html2canvas, vendored
// in app/static/js) before the modal opens, then hand off to
// app/view/issue_report.R via the input named in the button's data-report-input.

function sendReport(kind, message) {
  var navBtn = document.getElementById("report-issue");
  if (!navBtn || !window.Shiny) return;
  var inputId = navBtn.getAttribute("data-report-input");
  var post = function (shot) {
    Shiny.setInputValue(
      inputId,
      { kind: kind, message: message || "", screenshot: shot || "" },
      { priority: "event" }
    );
  };
  var capture = function () {
    window
      .html2canvas(document.body, { logging: false, scale: 1 })
      .then(function (canvas) { post(canvas.toDataURL("image/jpeg", 0.7)); })
      .catch(function () { post(""); });
  };
  if (typeof window.html2canvas === "function") return capture();
  // Only the visualization tab loads html2canvas; load it here when absent.
  var s = document.createElement("script");
  s.src = "static/js/html2canvas.min.js";
  s.onload = capture;
  s.onerror = function () { post(""); };
  document.head.appendChild(s);
}

function decorate(note) {
  if (note.querySelector(".report-issue-btn")) return;
  var btn = document.createElement("button");
  btn.type = "button";
  btn.className = "btn btn-sm btn-outline-danger report-issue-btn mt-1";
  btn.textContent = "Report";
  note.querySelector(".shiny-notification-content").appendChild(btn);
}

document.addEventListener("click", function (event) {
  if (event.target.closest("#report-issue")) {
    sendReport("inquiry", "");
    return;
  }
  var btn = event.target.closest(".report-issue-btn");
  if (!btn) return;
  var note = btn.closest(".shiny-notification");
  var text = note.querySelector(".shiny-notification-content-text");
  sendReport("error", text ? text.innerText.trim() : "");
});

// The notification panel is created lazily, so watch the whole body, but only
// look at added nodes rather than re-scanning the DOM on every mutation.
// The bundle loads in <head>, before <body> exists: observing earlier throws
// and aborts the rest of the bundle (including the splash-screen fade-out).
document.addEventListener("DOMContentLoaded", function () {
  new MutationObserver(function (mutations) {
    mutations.forEach(function (m) {
      m.addedNodes.forEach(function (n) {
        if (n.nodeType !== 1) return;
        var note = n.classList.contains("shiny-notification-error")
          ? n
          : n.closest && n.closest(".shiny-notification-error");
        if (note && note.querySelector(".shiny-notification-content")) decorate(note);
      });
    });
  }).observe(document.body, { childList: true, subtree: true });
});
