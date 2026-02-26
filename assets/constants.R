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

pubmlst_schemes <- data.frame(
  species = paste0(
    c(
      "Acinetobacter_baumannii",
      "Bacillus_anthracis",
      "Bacillus_cereus",
      "Borrelia_spp",
      "Brucella_spp",
      "Burkholderia_mallei",
      "Burkholderia_pseudomallei",
      "Campylobacter_jejuni_coli_v1",
      "Campylobacter_jejuni_coli_v2",
      "Chlamydia_abortus",
      "Chlamydia_trachomatis",
      "Clostridium_chauvoei",
      "Clostridium_perfringens",
      "Dichelobacter_nodosus",
      "Escherichia_spp",
      "Haemophilus_influenzae",
      "Leptospira_spp",
      "Mycobacteroides_abscessus_complex",
      "Neisseria_gonorrhoeae_v1",
      "Neisseria_gonorrhoeae_v2",
      "Neisseria_meningitidis_v1",
      "Neisseria_meningitidis_v2",
      "Neisseria_meningitidis_v3",
      "Human-restricted_Neisseria_v1",
      "Neisseria_L3",
      "Neisseria_L44",
      "Pasteurella_multocida",
      "Salmonella_v1",
      "Salmonella_v2_enterobase",
      "Serratia_spp",
      "Staphylococcus_aureus",
      "Streptococcus_agalactiae",
      "Streptococcus_pneumoniae",
      "Streptococcus_uberis",
      "Vibrio_cholerae",
      "Vibrio_parahaemolyticus",
      "Xanthomonas_citri"
    ),
    "_PM"
  ),
  url = c(
    'https://rest.pubmlst.org/db/pubmlst_abaumannii_seqdef/schemes/3',
    'https://rest.pubmlst.org/db/pubmlst_bcereus_seqdef/schemes/2',
    'https://rest.pubmlst.org/db/pubmlst_bcereus_seqdef/schemes/5',
    'https://rest.pubmlst.org/db/pubmlst_borrelia_seqdef/schemes/3',
    'https://rest.pubmlst.org/db/pubmlst_brucella_seqdef/schemes/3',
    'https://rest.pubmlst.org/db/pubmlst_bmallei_seqdef/schemes/1',
    'https://rest.pubmlst.org/db/pubmlst_bpseudomallei_seqdef/schemes/2',
    'https://rest.pubmlst.org/db/pubmlst_campylobacter_seqdef/schemes/4',
    'https://rest.pubmlst.org/db/pubmlst_campylobacter_seqdef/schemes/8',
    'https://rest.pubmlst.org/db/pubmlst_chlamydiales_seqdef/schemes/44',
    'https://rest.pubmlst.org/db/pubmlst_chlamydiales_seqdef/schemes/42',
    'https://rest.pubmlst.org/db/pubmlst_cchauvoei_seqdef/schemes/1',
    'https://rest.pubmlst.org/db/pubmlst_cperfringens_seqdef/schemes/2',
    'https://rest.pubmlst.org/db/pubmlst_dnodosus_seqdef/schemes/3',
    'https://rest.pubmlst.org/db/pubmlst_escherichia_seqdef/schemes/6',
    'https://rest.pubmlst.org/db/pubmlst_hinfluenzae_seqdef/schemes/56',
    'https://rest.pubmlst.org/db/pubmlst_leptospira_seqdef/schemes/4',
    'https://rest.pubmlst.org/db/pubmlst_mabscessus_seqdef/schemes/2',
    'https://rest.pubmlst.org/db/pubmlst_neisseria_seqdef/schemes/62',
    'https://rest.pubmlst.org/db/pubmlst_neisseria_seqdef/schemes/89',
    'https://rest.pubmlst.org/db/pubmlst_neisseria_seqdef/schemes/47',
    'https://rest.pubmlst.org/db/pubmlst_neisseria_seqdef/schemes/85',
    'https://rest.pubmlst.org/db/pubmlst_neisseria_seqdef/schemes/88',
    'https://rest.pubmlst.org/db/pubmlst_neisseria_seqdef/schemes/72',
    'https://rest.pubmlst.org/db/pubmlst_neisseria_seqdef/schemes/45',
    'https://rest.pubmlst.org/db/pubmlst_neisseria_seqdef/schemes/68',
    'https://rest.pubmlst.org/db/pubmlst_pmultocida_seqdef/schemes/3',
    'https://rest.pubmlst.org/db/pubmlst_salmonella_seqdef/schemes/4',
    'https://rest.pubmlst.org/db/pubmlst_salmonella_seqdef/schemes/3',
    'https://rest.pubmlst.org/db/pubmlst_serratia_seqdef/schemes/2',
    'https://rest.pubmlst.org/db/pubmlst_saureus_seqdef/schemes/20',
    'https://rest.pubmlst.org/db/pubmlst_sagalactiae_seqdef/schemes/38',
    'https://rest.pubmlst.org/db/pubmlst_spneumoniae_seqdef/schemes/2',
    'https://rest.pubmlst.org/db/pubmlst_suberis_seqdef/schemes/8',
    'https://rest.pubmlst.org/db/pubmlst_vcholerae_seqdef/schemes/3',
    'https://rest.pubmlst.org/db/pubmlst_vparahaemolyticus_seqdef/schemes/3',
    'https://rest.pubmlst.org/db/pubmlst_xcitri_seqdef/schemes/1'
  ),
  database = "pubMLST"
)

cgmlstorg_schemes <- data.frame(
  species = paste0(
    c(
      "Acinetobacter_baumannii",
      "Bacillus_anthracis",
      "Bordetella_pertussis",
      "Brucella_melitensis",
      "Brucella_spp",
      "Burkholderia_mallei_FLI",
      "Burkholderia_mallei_RKI",
      "Burkholderia_pseudomallei",
      "Campylobacter_jejuni_coli",
      "Clostridioides_difficile",
      "Clostridium_perfringens",
      "Corynebacterium_diphtheriae",
      "Corynebacterium_pseudotuberculosis",
      "Cronobacter_sakazakii_malonaticus",
      "Enterococcus_faecalis",
      "Enterococcus_faecium",
      "Escherichia_coli",
      "Francisella_tularensis",
      "Klebsiella_oxytoca_sensu_lato",
      "Klebsiella_pneumoniae_sensu_lato",
      "Legionella_pneumophila",
      "Listeria_monocytogenes",
      "Mycobacterium_tuberculosis_complex",
      "Mycobacteroides_abscessus",
      "Mycoplasma_gallisepticum",
      "Paenibacillus_larvae",
      "Pseudomonas_aeruginosa",
      "Salmonella_enterica",
      "Serratia_marcescens",
      "Staphylococcus_aureus",
      "Staphylococcus_capitis",
      "Streptococcus_pyogenes",
      "Yersinia_enterocolitica"
    ),
    "_CM"
  ),
  url = "",
  database = "cgMLST.org"
)

abb <- c(
  'Abaumannii',
  'Banthracis',
  'Bpertussis',
  'Bmelitensis',
  'Brucella',
  'Bmallei_fli',
  'Bmallei_rki',
  'Bpseudomallei',
  'Cjejuni',
  'Cdifficile',
  'Cperfringens',
  'Cdiphtheriae',
  'Cpseudotuberculosis',
  'Csakazakii',
  'Efaecalis',
  'Efaecium',
  'Ecoli',
  'Ftularensis',
  'Koxytoca',
  'Kpneumoniae',
  'Lpneumophila',
  'Lmonocytogenes',
  'Mtuberculosis',
  'Mabscessus',
  'Mgallisepticum',
  'Plarvae',
  'Paeruginosa',
  'Senterica',
  'Smarcescens',
  'Saureus',
  'Scapitis',
  'Spyogenes',
  'Yenterocolitica'
)

amrfinder_species <- c(
  "Acinetobacter_baumannii",
  "Burkholderia_cepacia",
  "Burkholderia_mallei",
  "Burkholderia_pseudomallei",
  "Campylobacter",
  "Citrobacter_freundii",
  "Clostridioides_difficile",
  "Corynebacterium_diphtheriae",
  "Enterobacter_asburiae",
  "Enterobacter_cloacae",
  "Enterococcus_faecalis",
  "Enterococcus_faecium",
  "Escherichia",
  "Klebsiella_oxytoca",
  "Klebsiella_pneumoniae",
  "Neisseria_gonorrhoeae",
  "Neisseria_meningitidis",
  "Pseudomonas_aeruginosa",
  "Salmonella",
  "Serratia_marcescens",
  "Staphylococcus_aureus",
  "Staphylococcus_pseudintermedius",
  "Streptococcus_agalactiae",
  "Streptococcus_pneumoniae",
  "Streptococcus_pyogenes",
  "Vibrio_cholerae",
  "Vibrio_parahaemolyticus",
  "Vibrio_vulnificus"
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

block_ui <-
  'document.getElementById("blocking-overlay").style.display = "block";'
unblock_ui <-
  'document.getElementById("blocking-overlay").style.display = "none";'

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
