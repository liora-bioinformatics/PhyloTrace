# Constants
phylotraceVersion <- paste("1.6.1")

sequential_scales <- list(
  "YlOrRd",
  "YlOrBr",
  "YlGnBu",
  "YlGn",
  "Reds",
  "RdPu",
  "Purples",
  "PuRd",
  "PuBuGn",
  "PuBu",
  "OrRd",
  "Oranges",
  "Greys",
  "Greens",
  "GnBu",
  "BuPu",
  "BuGn",
  "Blues"
)

qualitative_scales <- list(
  "Set1",
  "Set2",
  "Set3",
  "Pastel1",
  "Pastel2",
  "Paired",
  "Dark2",
  "Accent"
)

gradient_scales <- list(
  "Magma" = "magma",
  "Inferno" = "inferno",
  "Plasma" = "plasma",
  "Viridis" = "viridis",
  "Cividis" = "cividis",
  "Rocket" = "rocket",
  "Mako" = "mako",
  "Turbo" = "turbo"
)

diverging_scales <- list(
  "Spectral",
  "RdYlGn",
  "RdYlBu",
  "RdGy",
  "RdBu",
  "PuOr",
  "PRGn",
  "PiYG",
  "BrBG"
)

country_names <- c(
  "Afghanistan",
  "Albania",
  "Algeria",
  "Andorra",
  "Angola",
  "Antigua and Barbuda",
  "Argentina",
  "Armenia",
  "Australia",
  "Austria",
  "Azerbaijan",
  "Bahamas",
  "Bahrain",
  "Bangladesh",
  "Barbados",
  "Belarus",
  "Belgium",
  "Belize",
  "Benin",
  "Bhutan",
  "Bolivia",
  "Bosnia and Herzegovina",
  "Botswana",
  "Brazil",
  "Brunei",
  "Bulgaria",
  "Burkina Faso",
  "Burundi",
  "Côte d'Ivoire",
  "Cabo Verde",
  "Cambodia",
  "Cameroon",
  "Canada",
  "Central African Republic",
  "Chad",
  "Chile",
  "China",
  "Colombia",
  "Comoros",
  "Congo (Congo-Brazzaville)",
  "Costa Rica",
  "Croatia",
  "Cuba",
  "Cyprus",
  "Czechia (Czech Republic)",
  "Democratic Republic of the Congo",
  "Denmark",
  "Djibouti",
  "Dominica",
  "Dominican Republic",
  "Ecuador",
  "Egypt",
  "El Salvador",
  "Equatorial Guinea",
  "Eritrea",
  "Estonia",
  'Eswatini (fmr. "Swaziland")',
  "Ethiopia",
  "Fiji",
  "Finland",
  "France",
  "Gabon",
  "Gambia",
  "Georgia",
  "Germany",
  "Ghana",
  "Greece",
  "Grenada",
  "Guatemala",
  "Guinea",
  "Guinea-Bissau",
  "Guyana",
  "Haiti",
  "Holy See",
  "Honduras",
  "Hungary",
  "Iceland",
  "India",
  "Indonesia",
  "Iran",
  "Iraq",
  "Ireland",
  "Israel",
  "Italy",
  "Jamaica",
  "Japan",
  "Jordan",
  "Kazakhstan",
  "Kenya",
  "Kiribati",
  "Kuwait",
  "Kyrgyzstan",
  "Laos",
  "Latvia",
  "Lebanon",
  "Lesotho",
  "Liberia",
  "Libya",
  "Liechtenstein",
  "Lithuania",
  "Luxembourg",
  "Madagascar",
  "Malawi",
  "Malaysia",
  "Maldives",
  "Mali",
  "Malta",
  "Marshall Islands",
  "Mauritania",
  "Mauritius",
  "Mexico",
  "Micronesia",
  "Moldova",
  "Monaco",
  "Mongolia",
  "Montenegro",
  "Morocco",
  "Mozambique",
  "Myanmar (formerly Burma)",
  "Namibia",
  "Nauru",
  "Nepal",
  "Netherlands",
  "New Zealand",
  "Nicaragua",
  "Niger",
  "Nigeria",
  "North Korea",
  "North Macedonia (formerly Macedonia)",
  "Norway",
  "Oman",
  "Pakistan",
  "Palau",
  "Palestine State",
  "Panama",
  "Papua New Guinea",
  "Paraguay",
  "Peru",
  "Philippines",
  "Poland",
  "Portugal",
  "Qatar",
  "Romania",
  "Russia",
  "Rwanda",
  "Saint Kitts and Nevis",
  "Saint Lucia",
  "Saint Vincent and the Grenadines",
  "Samoa",
  "San Marino",
  "Sao Tome and Principe",
  "Saudi Arabia",
  "Senegal",
  "Serbia",
  "Seychelles",
  "Sierra Leone",
  "Singapore",
  "Slovakia",
  "Slovenia",
  "Solomon Islands",
  "Somalia",
  "South Africa",
  "South Korea",
  "South Sudan",
  "Spain",
  "Sri Lanka",
  "Sudan",
  "Suriname",
  "Sweden",
  "Switzerland",
  "Syria",
  "Tajikistan",
  "Tanzania",
  "Thailand",
  "Timor-Leste",
  "Togo",
  "Tonga",
  "Trinidad and Tobago",
  "Tunisia",
  "Turkey",
  "Turkmenistan",
  "Tuvalu",
  "Uganda",
  "Ukraine",
  "United Arab Emirates",
  "United Kingdom",
  "United States of America",
  "Uruguay",
  "Uzbekistan",
  "Vanuatu",
  "Venezuela",
  "Vietnam",
  "Yemen",
  "Zambia",
  "Zimbabwe"
)

sel_countries <- c(
  "Austria",
  "Germany",
  "Switzerland",
  "United Kingdom",
  "United States of America"
)
ctxRendererJS <- htmlwidgets::JS(
  "({ctx, id, x, y, state: { selected, hover }, style, font, label, metadata}) => {
    var pieData = JSON.parse(metadata);
    var radius = style.size;
    var centerX = x;
    var centerY = y;
    var total = pieData.reduce((sum, slice) => sum + slice.value, 0)
    var startAngle = 0;
    const drawNode = () => {
    // Set shadow properties
    if (style.shadow) {
    var shadowSize = style.shadowSize;
    ctx.shadowColor = style.shadowColor;
    ctx.shadowBlur = style.shadowSize;
    ctx.shadowOffsetX = style.shadowX;
    ctx.shadowOffsetY = style.shadowY;
    ctx.beginPath();
    ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI);
    ctx.fill();
    ctx.shadowColor = 'transparent';
    ctx.shadowBlur = 0;
    ctx.shadowOffsetX = 0;
    ctx.shadowOffsetY = 0;
    }
    pieData.forEach(slice => {
    var sliceAngle = 2 * Math.PI * (slice.value / total);
    ctx.beginPath();
    ctx.moveTo(centerX, centerY);
    ctx.arc(centerX, centerY, radius, startAngle, startAngle + sliceAngle);
    ctx.closePath();
    ctx.fillStyle = slice.color;
    ctx.fill();
    if (pieData.length > 1) {
    ctx.strokeStyle = 'black';
    ctx.lineWidth = 1;
    ctx.stroke();
    }
    startAngle += sliceAngle;
    });
    // Draw a border
    ctx.beginPath();
    ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI);
    ctx.strokeStyle = 'black';
    ctx.lineWidth = 1;
    ctx.stroke();
    };
    drawLabel = () => {
    //Draw the label
    var lines = label.split(`\n`);
    var lineHeight = font.size;
    ctx.font = `${font.size}px ${font.face}`;
    ctx.fillStyle = font.color;
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    lines.forEach((line, index) => {
    ctx.fillText(line, centerX, 
    centerY + radius + (index + 1) * lineHeight);
    })
    }
    return {
    drawNode,
    drawExternalLabel: drawLabel,
    nodeDimensions: { width: 2 * radius, height: 2 * radius },
    };
    }"
)
