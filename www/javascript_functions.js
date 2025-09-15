// toggle between plot view
function toggleCustomHeight() {
  let styleTag = document.getElementById('dynamic-tree-plot-style');
  if (!styleTag) {
    styleTag = document.createElement('style');
    styleTag.id = 'dynamic-tree-plot-style';
    document.head.appendChild(styleTag);
  }
  if (styleTag.innerHTML.includes('height: unset')) {
    styleTag.innerHTML = `
      #tree_plot img { height: 70vh !important; border-radius: 0; }
      #tree_plot { border-radius: 0; }
      #tree_field > div { border-radius: 0; }
      #tree_field { border: 1px #ffbf00 solid; }
      #tree_aspect { color: #ffbf00; background: #00000087; }
    `;
  } else {
    styleTag.innerHTML = `
      #tree_plot img { height: unset !important; border-radius: unset; }
      #tree_plot { border-radius: 5px; }
      #tree_field > div { border-radius: 5px; }
      #tree_field { border: none; }
      #tree_aspect { color: transparent; background: transparent; }
    `;
  }
}

// highlighting pin dataset icon
function highlight_pin() {
  let iconElement = document.querySelector("#menu_typing > li.treeview > ul > li.active > a > i");

  if (iconElement) {
    // Change the icon class
    iconElement.classList.remove("fas", "fa-angles-right");
    iconElement.classList.add("fa-solid", "fa-map-pin", "attention-animation-tab");
  }
}

function unhighlight_pin() {
  let iconElement = document.querySelector("#menu_typing > li.treeview > ul > li.active > a > i");

  if (iconElement) {
    // Change the icon class
    iconElement.classList.remove("fa-solid", "fa-map-pin", "attention-animation-tab");
    iconElement.classList.add("fas", "fa-angles-right");
  }
}

// highlighting typing icon
function highlight_typing() {
  let iconElement = document.querySelector("#menu_typing > li.active > a > i");

  if (iconElement) {
    // Change the icon class
    iconElement.classList.remove("fas", "fa-gears");
    iconElement.classList.add("fa-solid", "fa-gears", "fa-spin-pulse");
  }
}

function unhighlight_typing() {
  let iconElement = document.querySelector("#menu_typing > li.active > a > i");

  if (iconElement) {
    // Change the icon class
    iconElement.classList.remove("fa-solid", "fa-gears", "fa-spin-pulse");
    iconElement.classList.add("fas", "fa-gears");
  }
}

// highlighting clicked plot control buttons
Shiny.addCustomMessageHandler('nj_highlight', function (id) {
  document.getElementById(id).style.backgroundColor = '#1f4e5d';
});

Shiny.addCustomMessageHandler('nj_reset_style', function (id) {
  document.getElementById('nj_label_menu').style.backgroundColor = '';
  document.getElementById('nj_variable_menu').style.backgroundColor = '';
  document.getElementById('nj_color_menu').style.backgroundColor = '';
  document.getElementById('nj_elements_menu').style.backgroundColor = '';
  document.getElementById('nj_misc_menu').style.backgroundColor = '';
  document.getElementById('nj_download_menu').style.backgroundColor = '';
});

Shiny.addCustomMessageHandler('mst_highlight', function (id) {
  document.getElementById(id).style.backgroundColor = '#1f4e5d';
});

Shiny.addCustomMessageHandler('mst_reset_style', function (id) {
  document.getElementById('mst_label_menu').style.backgroundColor = '';
  document.getElementById('mst_variable_menu').style.backgroundColor = '';
  document.getElementById('mst_color_menu').style.backgroundColor = '';
  document.getElementById('mst_size_menu').style.backgroundColor = '';
  document.getElementById('mst_misc_menu').style.backgroundColor = '';
  document.getElementById('mst_download_menu').style.backgroundColor = '';
});

// showing and hiding the blocking overlay during computation
Shiny.addCustomMessageHandler('show_overlay', function (message) {
  document.getElementById('blocking-overlay').style.display = 'block';
});

Shiny.addCustomMessageHandler('hide_overlay', function (message) {
  document.getElementById('blocking-overlay').style.display = 'none';
});

// Functions to find the index of the selected cgMLST option
function findSelectedIndex() {
  // Ensure the element exists before proceeding
  var options = $('#select_cgmlst option');
  if (options.length === 0) return; // Exit if options are not found

  var selectedIndex = -1;

  // Iterate over the options to find which one is selected
  options.each(function (index) {
    if ($(this).prop('selected') === true) {
      selectedIndex = index;  // Store the index of the selected option
      return false;  // Exit the loop once the selected option is found
    }
  });

  Shiny.setInputValue('selected_index_js', selectedIndex + 1);  // Adding 1 for 1-based index
}

// Attach the function to run whenever the selection changes
$(document).on('change', '#select_cgmlst', findSelectedIndex);

// Also run the function when the document is ready (in case there's an initial selection)
$(document).ready(function () {
  if ($('#select_cgmlst').length) {
    findSelectedIndex(); // Run it once on load
  }
});

// Copy sequence clipboard
Shiny.addCustomMessageHandler('txt', function (txt) {
  navigator.clipboard.writeText(txt);
});
// Copy hash clipboard
Shiny.addCustomMessageHandler('txt', function (txt) {
  navigator.clipboard.writeText(txt);
});


// Get date format
function getCurrentDate() {
  var currentDate = new Date();
  var year = currentDate.getFullYear();

  // Get month and day with padding since JavaScript months are 0-based
  var month = String(currentDate.getMonth() + 1).padStart(2, '0'); // Adds leading 0 if needed
  var day = String(currentDate.getDate()).padStart(2, '0'); // Adds leading 0 if needed

  return year + '-' + month + '-' + day; // Formats the date as "YYYY-MM-DD"
};

function checkAndExecute() {
  if (document.querySelector("#loaded_scheme > div > li > span") !== null) {
    // Select the span element
    let spanElement = document.querySelector("#loaded_scheme > div > li > span");

    // Get the bounding rectangle of the span element
    let rect = spanElement.getBoundingClientRect();

    // Extract the width
    let width = rect.width;

    Shiny.setInputValue("scheme_position", width);
  }
}
setInterval(checkAndExecute, 1000);
checkAndExecute();

// Get time
function updateTime() {
  var options = {
    timeZone: Intl.DateTimeFormat().resolvedOptions().timeZone,
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit',
    hour12: false,
    timeZoneName: 'short'
  };
  var currentTime = new Date().toLocaleTimeString([], options);
  document.getElementById('currentTime').innerHTML = currentTime;

}
setInterval(updateTime, 1000);
updateTime();

//MST png to report
function mstReport() {
  var canvases = document.querySelectorAll('canvas');

  if (canvases.length < 2) {
    // Get all canvas elements on the document
    var canvas = document.querySelector('canvas');

    // Assuming the canvases are ordered as title, subtitle, main
    var titleCanvasID = document.getElementById('titletree_mst');
    if (!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = canvas.width;
      titleCanvas.height = titleCanvasHeight;

      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('text-align');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 2, titleCanvasHeight);
    } else { var titleCanvasHeight = 0; }
    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if (!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = canvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;

      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 2, subtitleCanvasHeight);
    } else { var subtitleCanvasHeight = 0; }

    // Get the heights of the canvas
    var mainCanvasHeight = canvas.height;

    // Get the main canvas context
    var mainCtx = canvas.getContext('2d');
    var mainCanvasWidth = canvas.width;

    // Create a new canvas to merge title, subtitle, main plot
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = canvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;

    var ctx = mergedCanvas.getContext('2d');

    // Draw title, subtitle, main plot onto the merged canvas
    var bgColor = getBackgroundColor()
    if (bgColor === 'rgba(0, 0, 0, 0)') {
      ctx.fillStyle = 'rgb(255, 255, 255)';
    } else {
      ctx.fillStyle = bgColor;
    }
    ctx.fillRect(0, 0, canvas.width, mergedCanvas.height);
    if (!(titleCanvasID.innerText.length === 0)) {
      ctx.drawImage(titleCanvas, 0, 0);
    }
    if (!(subtitleCanvasID.innerText.length === 0)) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }
    ctx.drawImage(canvas, 0, titleCanvasHeight + subtitleCanvasHeight);
  } else {
    var legendCanvas = canvases[0];
    var plotCanvas = canvases[1];

    // Assuming the canvases are ordered as title, subtitle, main
    var titleCanvasID = document.getElementById('titletree_mst');
    if (!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = legendCanvas.width + plotCanvas.width;
      titleCanvas.height = titleCanvasHeight;

      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('left');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 20, titleCanvasHeight / 1.5);
    } else { var titleCanvasHeight = 0; }

    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if (!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = legendCanvas.width + plotCanvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;

      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 20, subtitleCanvasHeight / 1.5);
    } else { var subtitleCanvasHeight = 0; }

    // Get the heights of the canvas
    var mainCanvasHeight = plotCanvas.height;

    // Create a new canvas to merge title, subtitle, main plot
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = legendCanvas.width + plotCanvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;
    var ctx = mergedCanvas.getContext('2d');

    // Draw background
    var bgColor = getBackgroundColor()
    if (bgColor === 'rgba(0, 0, 0, 0)') {
      ctx.fillStyle = 'rgb(255, 255, 255)';
    } else {
      ctx.fillStyle = bgColor;
    }
    ctx.fillRect(0, 0, mergedCanvas.width, mergedCanvas.height);

    // Draw title
    if (titleCanvasID && titleCanvasID.innerText.length !== 0) {
      ctx.drawImage(titleCanvas, 0, 0);
    }

    // Draw subtitle
    if (subtitleCanvasID && subtitleCanvasID.innerText.length !== 0) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }

    // Draw legend and plot canvases side by side
    ctx.drawImage(legendCanvas, 0, titleCanvasHeight + subtitleCanvasHeight);
    ctx.drawImage(plotCanvas, legendCanvas.width, titleCanvasHeight + subtitleCanvasHeight);
  }

  // Convert the canvas to a data URL
  var dataURL = mergedCanvas.toDataURL('image/jpeg', 1.0);

  // Extract the base64-encoded string
  var base64Data = dataURL.split(',')[1];

  console.log(base64Data);

  // Send the base64-encoded string to the server
  Shiny.setInputValue('canvas_data', base64Data);

  mergedCanvas.remove();
}

// MST jpeg download
$(document).on('click', '#save_plot_jpeg', function () {

  var canvases = document.querySelectorAll('canvas');

  if (canvases.length < 2) {
    // Get all canvas elements on the document
    var canvas = document.querySelector('canvas');

    // Assuming the canvases are ordered as title, subtitle, main
    var titleCanvasID = document.getElementById('titletree_mst');
    if (!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = canvas.width;
      titleCanvas.height = titleCanvasHeight;

      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('text-align');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 2, titleCanvasHeight);
    } else { var titleCanvasHeight = 0; }
    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if (!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = canvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;

      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 2, subtitleCanvasHeight);
    } else { var subtitleCanvasHeight = 0; }

    // Get the heights of the canvas
    var mainCanvasHeight = canvas.height;

    // Get the main canvas context
    var mainCtx = canvas.getContext('2d');
    var mainCanvasWidth = canvas.width;

    // Create a new canvas to merge title, subtitle, main plot
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = canvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;

    var ctx = mergedCanvas.getContext('2d');

    // Draw title, subtitle, main plot onto the merged canvas
    var bgColor = getBackgroundColor()
    if (bgColor === 'rgba(0, 0, 0, 0)') {
      ctx.fillStyle = 'rgb(255, 255, 255)';
    } else {
      ctx.fillStyle = bgColor;
    }
    ctx.fillRect(0, 0, canvas.width, mergedCanvas.height);
    if (!(titleCanvasID.innerText.length === 0)) {
      ctx.drawImage(titleCanvas, 0, 0);
    }
    if (!(subtitleCanvasID.innerText.length === 0)) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }
    ctx.drawImage(canvas, 0, titleCanvasHeight + subtitleCanvasHeight);
  } else {
    var legendCanvas = canvases[0];
    var plotCanvas = canvases[1];

    // Assuming the canvases are ordered as title, subtitle, main
    var titleCanvasID = document.getElementById('titletree_mst');
    if (!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = legendCanvas.width + plotCanvas.width;
      titleCanvas.height = titleCanvasHeight;

      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('left');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 20, titleCanvasHeight / 1.5);
    } else { var titleCanvasHeight = 0; }

    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if (!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = legendCanvas.width + plotCanvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;

      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 20, subtitleCanvasHeight / 1.5);
    } else { var subtitleCanvasHeight = 0; }

    // Get the heights of the canvas
    var mainCanvasHeight = plotCanvas.height;

    // Create a new canvas to merge title, subtitle, main plot
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = legendCanvas.width + plotCanvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;
    var ctx = mergedCanvas.getContext('2d');

    // Draw background
    var bgColor = getBackgroundColor()
    if (bgColor === 'rgba(0, 0, 0, 0)') {
      ctx.fillStyle = 'rgb(255, 255, 255)';
    } else {
      ctx.fillStyle = bgColor;
    }
    ctx.fillRect(0, 0, mergedCanvas.width, mergedCanvas.height);

    // Draw title
    if (titleCanvasID && titleCanvasID.innerText.length !== 0) {
      ctx.drawImage(titleCanvas, 0, 0);
    }

    // Draw subtitle
    if (subtitleCanvasID && subtitleCanvasID.innerText.length !== 0) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }

    // Draw legend and plot canvases side by side
    ctx.drawImage(legendCanvas, 0, titleCanvasHeight + subtitleCanvasHeight);
    ctx.drawImage(plotCanvas, legendCanvas.width, titleCanvasHeight + subtitleCanvasHeight);
  }

  // Download the merged canvas as a JPEG image
  const a = document.createElement('a');
  document.body.append(a);
  a.download = getCurrentDate() + '_MST';
  a.href = mergedCanvas.toDataURL('image/jpeg', 1.0);
  a.click();
  a.remove();

  // Remove the dynamically created merged canvas
  mergedCanvas.remove();

});

// MST png Download
$(document).on('click', '#save_plot_png', function () {

  var canvases = document.querySelectorAll('canvas');

  if (canvases.length < 2) {
    // Get all canvas elements on the document
    var canvas = document.querySelector('canvas');

    // Assuming the canvases are ordered as title, subtitle, main
    var titleCanvasID = document.getElementById('titletree_mst');
    if (!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = canvas.width;
      titleCanvas.height = titleCanvasHeight;

      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('text-align');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 2, titleCanvasHeight);
    } else { var titleCanvasHeight = 0; }
    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if (!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = canvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;

      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 2, subtitleCanvasHeight);
    } else { var subtitleCanvasHeight = 0; }

    // Get the heights of the canvas
    var mainCanvasHeight = canvas.height;

    // Get the main canvas context
    var mainCtx = canvas.getContext('2d');
    var mainCanvasWidth = canvas.width;

    // Create a new canvas to merge title, subtitle, main plot
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = canvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;

    var ctx = mergedCanvas.getContext('2d');

    // Draw title, subtitle, main plot onto the merged canvas
    ctx.fillStyle = getBackgroundColor();
    ctx.fillRect(0, 0, canvas.width, mergedCanvas.height);
    if (!(titleCanvasID.innerText.length === 0)) {
      ctx.drawImage(titleCanvas, 0, 0);
    }
    if (!(subtitleCanvasID.innerText.length === 0)) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }
    ctx.drawImage(canvas, 0, titleCanvasHeight + subtitleCanvasHeight);
  } else {
    var legendCanvas = canvases[0];
    var plotCanvas = canvases[1];

    // Assuming the canvases are ordered as title, subtitle, main
    var titleCanvasID = document.getElementById('titletree_mst');
    if (!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = legendCanvas.width + plotCanvas.width;
      titleCanvas.height = titleCanvasHeight;

      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('left');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 20, titleCanvasHeight / 1.5);
    } else { var titleCanvasHeight = 0; }

    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if (!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = legendCanvas.width + plotCanvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;

      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 20, subtitleCanvasHeight / 1.5);
    } else { var subtitleCanvasHeight = 0; }

    // Get the heights of the canvas
    var mainCanvasHeight = plotCanvas.height;

    // Create a new canvas to merge title, subtitle, main plot
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = legendCanvas.width + plotCanvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;
    var ctx = mergedCanvas.getContext('2d');

    // Draw background
    ctx.fillStyle = getBackgroundColor();
    ctx.fillRect(0, 0, mergedCanvas.width, mergedCanvas.height);

    // Draw title
    if (titleCanvasID && titleCanvasID.innerText.length !== 0) {
      ctx.drawImage(titleCanvas, 0, 0);
    }

    // Draw subtitle
    if (subtitleCanvasID && subtitleCanvasID.innerText.length !== 0) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }

    // Draw legend and plot canvases side by side
    ctx.drawImage(legendCanvas, 0, titleCanvasHeight + subtitleCanvasHeight);
    ctx.drawImage(plotCanvas, legendCanvas.width, titleCanvasHeight + subtitleCanvasHeight);
  }

  // Download the merged canvas as a PNG image
  const a = document.createElement('a');
  document.body.append(a);
  a.download = getCurrentDate() + '_MST.png';
  a.href = mergedCanvas.toDataURL('image/png');
  a.click();
  a.remove();

  // Remove the dynamically created merged canvas
  mergedCanvas.remove();

});

// MST bmp Download
$(document).on('click', '#save_plot_bmp', function () {

  var canvases = document.querySelectorAll('canvas');

  if (canvases.length < 2) {
    // Get all canvas elements on the document
    var canvas = document.querySelector('canvas');

    // Assuming the canvases are ordered as title, subtitle, main
    var titleCanvasID = document.getElementById('titletree_mst');
    if (!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = canvas.width;
      titleCanvas.height = titleCanvasHeight;

      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('text-align');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 2, titleCanvasHeight);
    } else { var titleCanvasHeight = 0; }
    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if (!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = canvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;

      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 2, subtitleCanvasHeight);
    } else { var subtitleCanvasHeight = 0; }

    // Get the heights of the canvas
    var mainCanvasHeight = canvas.height;

    // Get the main canvas context
    var mainCtx = canvas.getContext('2d');
    var mainCanvasWidth = canvas.width;

    // Create a new canvas to merge title, subtitle, main plot
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = canvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;

    var ctx = mergedCanvas.getContext('2d');

    // Draw title, subtitle, main plot onto the merged canvas
    var bgColor = getBackgroundColor()
    if (bgColor === 'rgba(0, 0, 0, 0)') {
      ctx.fillStyle = 'rgb(255, 255, 255)';
    } else {
      ctx.fillStyle = bgColor;
    }
    ctx.fillRect(0, 0, canvas.width, mergedCanvas.height);
    if (!(titleCanvasID.innerText.length === 0)) {
      ctx.drawImage(titleCanvas, 0, 0);
    }
    if (!(subtitleCanvasID.innerText.length === 0)) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }
    ctx.drawImage(canvas, 0, titleCanvasHeight + subtitleCanvasHeight);
  } else {
    var legendCanvas = canvases[0];
    var plotCanvas = canvases[1];

    // Assuming the canvases are ordered as title, subtitle, main
    var titleCanvasID = document.getElementById('titletree_mst');
    if (!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = legendCanvas.width + plotCanvas.width;
      titleCanvas.height = titleCanvasHeight;

      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('left');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 20, titleCanvasHeight / 1.5);
    } else { var titleCanvasHeight = 0; }

    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if (!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = legendCanvas.width + plotCanvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;

      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 20, subtitleCanvasHeight / 1.5);
    } else { var subtitleCanvasHeight = 0; }

    // Get the heights of the canvas
    var mainCanvasHeight = plotCanvas.height;

    // Create a new canvas to merge title, subtitle, main plot
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = legendCanvas.width + plotCanvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;
    var ctx = mergedCanvas.getContext('2d');

    // Draw background
    var bgColor = getBackgroundColor()
    if (bgColor === 'rgba(0, 0, 0, 0)') {
      ctx.fillStyle = 'rgb(255, 255, 255)';
    } else {
      ctx.fillStyle = bgColor;
    }
    ctx.fillRect(0, 0, mergedCanvas.width, mergedCanvas.height);

    // Draw title
    if (titleCanvasID && titleCanvasID.innerText.length !== 0) {
      ctx.drawImage(titleCanvas, 0, 0);
    }

    // Draw subtitle
    if (subtitleCanvasID && subtitleCanvasID.innerText.length !== 0) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }

    // Draw legend and plot canvases side by side
    ctx.drawImage(legendCanvas, 0, titleCanvasHeight + subtitleCanvasHeight);
    ctx.drawImage(plotCanvas, legendCanvas.width, titleCanvasHeight + subtitleCanvasHeight);
  }

  // Download the merged canvas as a bmp image
  const a = document.createElement('a');
  document.body.append(a);
  a.download = getCurrentDate() + '_MST.bmp';
  a.href = mergedCanvas.toDataURL('image/bmp');
  a.click();
  a.remove();
  // Remove the dynamically created merged canvas
  mergedCanvas.remove();

});

// Function to get the RGB color from a specific sub-element with a specific ID and class
function getBackgroundColorClear() {
  // Check the state of the Shiny checkbox 
  var checkboxStatus = $('#mst_background_transparent').prop('checked');

  // If the checkbox is checked, return transparent RGB code
  if (checkboxStatus) {
    return 'rgba(0, 0, 0, 0)';
  } else {
    // Get the sub-element by ID and class
    var targetElement = document.querySelector('#mst_background_color input.form-control.pickr-color');

    // Check if the element is found
    if (targetElement) {
      // Get the computed style of the element
      var computedStyle = window.getComputedStyle(targetElement);

      // Get the background color property
      var backgroundColor = computedStyle.backgroundColor;

      // Parse the RGB values from the string
      var rgbArray = backgroundColor.match(/\d+/g);

      // Convert the RGB values to a formatted string
      var rgbString = 'rgb(' + rgbArray.join(', ') + ')';

      // Return the RGB string
      return rgbString;
    } else {
      console.error('Element not found.');
      return null; // or any default value you want to return
    }
  }
};

// Function to get the RGB color from a specific sub-element with a specific ID and class
function getBackgroundColor() {

  // Select the target element by ID
  var targetElement = document.getElementById("tree_mst");

  // Check if the element exists
  if (targetElement) {
    // Get the computed style of the element
    var computedStyle = window.getComputedStyle(targetElement);

    // Retrieve the background color property from the computed style
    var backgroundColor = computedStyle.backgroundColor;

    // Return the background color value
    return backgroundColor;
  } else {
    console.error('Element with ID "tree_mst" not found.');
    return null; // Return null if the element does not exist
  }
};

document.addEventListener("input", function (event) {
  if ((event.target.tagName === "INPUT" && event.target.type === "text") || event.target.classList.contains("handsontableInput")) {
    const regex = /^[a-zA-Z0-9_-]*$/;
    if (!regex.test(event.target.value)) {
      event.target.value = event.target.value.replace(/[^a-zA-Z0-9_-]/g, "");
    }
  }
});

