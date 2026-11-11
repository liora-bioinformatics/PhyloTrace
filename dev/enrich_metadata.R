# dev/enrich_metadata.R
#
# Populate a PhyloTrace database's `metadata` table with realistic, diverse
# artificial example data. Handy for demos, screenshots and for exercising the
# visualisation views (map, timeline, filters) against databases that ship with
# an empty metadata table.
#
# Design goals:
#   * Diversity       — many distinct cities / countries, specimen types,
#                       dates, institutions, so filters and the map have plenty
#                       to work with.
#   * Congruence      — the spatial fields are internally consistent: city,
#                       state / province, country and postal code always come
#                       from the *same* real place, so you never get a German
#                       postcode on a Brazilian isolate. city + state + country
#                       also resolve cleanly in the map view's Nominatim
#                       geocoder (see app/view/visualization_map.R).
#   * Non-destructive — by default only *empty* cells are filled; existing
#                       values (notably `isolate` and `organism`) are preserved.
#                       Pass overwrite = TRUE to regenerate every enrichable
#                       cell instead.
#
# Reusable:  source("dev/enrich_metadata.R"); enrich_metadata("path/to.db")
#            enrich_metadata("path/to.db", continent = "Europe")
# One-shot:  Rscript dev/enrich_metadata.R path/to.db \
#              [--overwrite] [--seed 42] [--continent Europe] \
#              [--coordinates] [--bad-geo 8]
#
# --coordinates also fills geo_loc_coordinates with a point near each city
# centre (about 10% of rows are left empty, so geocoding still gets used);
# --bad-geo N additionally breaks N of those rows on purpose (swapped, in
# another country, unreadable, unknown country) to show the map's advisories.

library(DBI)
library(RSQLite)

# --- reference data --------------------------------------------------------

# One row per (country, state/province, city, postal code). Several postcodes
# per city and several cities per country give within- and between-country
# diversity while every row stays internally congruent. Postcodes use each
# country's real format; the map geocodes on city + state + country, so the
# postcode only needs to be plausible for the country, not pinpoint-accurate.
loc <- function(country, state, city, postal) {
  data.frame(
    geo_loc_name_country = country,
    geo_loc_name_state_province = state,
    geo_loc_name_city = city,
    geo_loc_name_postal_code = postal,
    stringsAsFactors = FALSE
  )
}

location_table <- rbind(
  # --- Europe --------------------------------------------------------------
  # Germany (5-digit)
  loc("Germany", "Berlin", "Berlin", c("10115", "10247", "12043")),
  loc("Germany", "Bavaria", "Munich", c("80331", "80802", "81667")),
  loc("Germany", "Bavaria", "Nuremberg", c("90402", "90478")),
  loc("Germany", "Hamburg", "Hamburg", c("20095", "20259", "22767")),
  loc(
    "Germany",
    "North Rhine-Westphalia",
    "Cologne",
    c("50667", "50823", "51063")
  ),
  loc("Germany", "North Rhine-Westphalia", "Dusseldorf", c("40213", "40474")),
  loc("Germany", "Hesse", "Frankfurt", c("60311", "60329", "60594")),
  loc("Germany", "Saxony", "Dresden", c("01067", "01097")),
  loc("Germany", "Saxony", "Leipzig", c("04109", "04315")),
  loc("Germany", "Baden-Wurttemberg", "Stuttgart", c("70173", "70567")),
  loc("Germany", "Lower Saxony", "Hanover", c("30159", "30451")),
  # United Kingdom (alphanumeric)
  loc(
    "United Kingdom",
    "England",
    "London",
    c("SW1A 1AA", "EC1A 1BB", "WC2N 5DU")
  ),
  loc("United Kingdom", "England", "Manchester", c("M1 1AE", "M13 9PL")),
  loc("United Kingdom", "England", "Birmingham", c("B1 1BB", "B15 2TT")),
  loc("United Kingdom", "England", "Leeds", c("LS1 4DY", "LS2 9JT")),
  loc("United Kingdom", "England", "Liverpool", c("L1 8JQ", "L3 5TF")),
  loc("United Kingdom", "England", "Bristol", c("BS1 4DJ", "BS8 1TH")),
  loc(
    "United Kingdom",
    "England",
    "Newcastle upon Tyne",
    c("NE1 4ST", "NE2 4HH")
  ),
  loc("United Kingdom", "England", "Sheffield", c("S1 2HE", "S10 2TN")),
  loc("United Kingdom", "Scotland", "Edinburgh", c("EH1 1BB", "EH8 9YL")),
  loc("United Kingdom", "Scotland", "Glasgow", c("G1 1XW", "G12 8QQ")),
  loc("United Kingdom", "Scotland", "Aberdeen", c("AB10 1YN", "AB24 3FX")),
  loc("United Kingdom", "Wales", "Cardiff", c("CF10 1EP", "CF14 4XW")),
  loc("United Kingdom", "Wales", "Swansea", c("SA1 3SN", "SA2 8PP")),
  loc("United Kingdom", "Northern Ireland", "Belfast", c("BT1 5GS", "BT9 5AH")),
  # France (5-digit)
  loc("France", "Ile-de-France", "Paris", c("75001", "75013", "75015")),
  loc("France", "Auvergne-Rhone-Alpes", "Lyon", c("69001", "69003")),
  loc("France", "Provence-Alpes-Cote d'Azur", "Marseille", c("13001", "13005")),
  loc("France", "Provence-Alpes-Cote d'Azur", "Nice", c("06000", "06100")),
  loc("France", "Occitanie", "Toulouse", c("31000", "31300")),
  loc("France", "Nouvelle-Aquitaine", "Bordeaux", c("33000", "33300")),
  loc("France", "Hauts-de-France", "Lille", c("59000", "59800")),
  loc("France", "Pays de la Loire", "Nantes", c("44000", "44200")),
  loc("France", "Grand Est", "Strasbourg", c("67000", "67100")),
  # Spain (5-digit)
  loc("Spain", "Community of Madrid", "Madrid", c("28001", "28013", "28040")),
  loc("Spain", "Catalonia", "Barcelona", c("08001", "08028")),
  loc("Spain", "Valencian Community", "Valencia", c("46001", "46010")),
  loc("Spain", "Andalusia", "Seville", c("41001", "41013")),
  loc("Spain", "Andalusia", "Malaga", c("29001", "29010")),
  loc("Spain", "Basque Country", "Bilbao", c("48001", "48009")),
  loc("Spain", "Aragon", "Zaragoza", c("50001", "50008")),
  # Italy (5-digit)
  loc("Italy", "Lazio", "Rome", c("00118", "00184")),
  loc("Italy", "Lombardy", "Milan", c("20121", "20133")),
  loc("Italy", "Campania", "Naples", c("80121", "80138")),
  loc("Italy", "Piedmont", "Turin", c("10121", "10126")),
  loc("Italy", "Emilia-Romagna", "Bologna", c("40121", "40126")),
  loc("Italy", "Tuscany", "Florence", c("50122", "50139")),
  loc("Italy", "Sicily", "Palermo", c("90133", "90139")),
  loc("Italy", "Liguria", "Genoa", c("16121", "16128")),
  # Netherlands (4-digit + 2 letters)
  loc("Netherlands", "North Holland", "Amsterdam", c("1012 JS", "1091 GC")),
  loc("Netherlands", "Utrecht", "Utrecht", c("3511 LN", "3584 CX")),
  loc("Netherlands", "South Holland", "Rotterdam", c("3011 AA", "3062 PA")),
  loc("Netherlands", "South Holland", "The Hague", c("2511 CV", "2595 AA")),
  loc("Netherlands", "North Brabant", "Eindhoven", c("5611 AZ", "5612 AB")),
  loc("Netherlands", "Groningen", "Groningen", c("9711 AA", "9712 CP")),
  # Belgium (4-digit)
  loc("Belgium", "Brussels-Capital Region", "Brussels", c("1000", "1050")),
  loc("Belgium", "Flanders", "Antwerp", c("2000", "2018")),
  loc("Belgium", "Flanders", "Ghent", c("9000", "9050")),
  loc("Belgium", "Flanders", "Bruges", c("8000", "8310")),
  loc("Belgium", "Wallonia", "Liege", c("4000", "4020")),
  # Switzerland (4-digit)
  loc("Switzerland", "Zurich", "Zurich", c("8001", "8091")),
  loc("Switzerland", "Geneva", "Geneva", c("1201", "1205")),
  loc("Switzerland", "Basel-Stadt", "Basel", c("4001", "4031")),
  loc("Switzerland", "Bern", "Bern", c("3011", "3013")),
  loc("Switzerland", "Vaud", "Lausanne", c("1003", "1005")),
  loc("Switzerland", "Lucerne", "Lucerne", c("6003", "6004")),
  # Austria (4-digit)
  loc("Austria", "Vienna", "Vienna", c("1010", "1090")),
  loc("Austria", "Styria", "Graz", c("8010", "8020")),
  loc("Austria", "Salzburg", "Salzburg", c("5020", "5023")),
  loc("Austria", "Tyrol", "Innsbruck", c("6020", "6060")),
  loc("Austria", "Upper Austria", "Linz", c("4020", "4040")),
  # Nordics (4-5 digit)
  loc("Sweden", "Stockholm", "Stockholm", c("111 29", "171 76")),
  loc("Sweden", "Vastra Gotaland", "Gothenburg", c("411 03", "413 45")),
  loc("Sweden", "Skane", "Malmo", c("211 34", "214 22")),
  loc("Sweden", "Uppsala", "Uppsala", c("753 09", "752 36")),
  loc("Denmark", "Capital Region", "Copenhagen", c("1050", "2100")),
  loc("Denmark", "Central Denmark Region", "Aarhus", c("8000", "8200")),
  loc("Denmark", "Region of Southern Denmark", "Odense", c("5000", "5230")),
  loc("Norway", "Oslo", "Oslo", c("0150", "0250")),
  loc("Norway", "Vestland", "Bergen", c("5003", "5020")),
  loc("Norway", "Trondelag", "Trondheim", c("7010", "7030")),
  loc("Finland", "Uusimaa", "Helsinki", c("00100", "00250")),
  loc("Finland", "Pirkanmaa", "Tampere", c("33100", "33720")),
  loc("Finland", "Southwest Finland", "Turku", c("20100", "20520")),
  # Poland (XX-XXX)
  loc("Poland", "Masovian Voivodeship", "Warsaw", c("00-001", "00-950")),
  loc("Poland", "Lesser Poland Voivodeship", "Krakow", c("30-001", "31-008")),
  loc("Poland", "Lower Silesian Voivodeship", "Wroclaw", c("50-001", "50-357")),
  loc("Poland", "Pomeranian Voivodeship", "Gdansk", c("80-001", "80-244")),
  loc("Poland", "Greater Poland Voivodeship", "Poznan", c("60-001", "61-701")),
  # Portugal (XXXX-XXX)
  loc("Portugal", "Lisbon District", "Lisbon", c("1100-148", "1250-096")),
  loc("Portugal", "Porto District", "Porto", c("4000-001", "4050-115")),
  loc("Portugal", "Coimbra District", "Coimbra", c("3000-001", "3030-320")),
  loc("Portugal", "Braga District", "Braga", c("4700-001", "4710-057")),
  # Ireland (Eircode)
  loc("Ireland", "Leinster", "Dublin", c("D01 F5P2", "D02 X285")),
  loc("Ireland", "Munster", "Cork", c("T12 X70N", "T23 XY41")),
  loc("Ireland", "Connacht", "Galway", c("H91 TK33", "H91 CF50")),
  loc("Ireland", "Munster", "Limerick", c("V94 T2XY", "V94 X5F8")),
  # Greece / Czechia (XXX XX)
  loc("Greece", "Attica", "Athens", c("105 57", "115 21")),
  loc("Greece", "Central Macedonia", "Thessaloniki", c("546 21", "546 36")),
  loc("Greece", "Western Greece", "Patras", c("262 21", "263 32")),
  loc("Czech Republic", "Prague", "Prague", c("110 00", "120 00")),
  loc("Czech Republic", "South Moravian Region", "Brno", c("602 00", "613 00")),
  loc(
    "Czech Republic",
    "Moravian-Silesian Region",
    "Ostrava",
    c("702 00", "708 00")
  ),
  # Turkey (5-digit)
  loc("Turkey", "Istanbul", "Istanbul", c("34000", "34381")),
  loc("Turkey", "Ankara", "Ankara", c("06000", "06420")),
  loc("Turkey", "Izmir", "Izmir", c("35000", "35220")),
  loc("Turkey", "Antalya", "Antalya", c("07000", "07070")),

  # --- Americas ------------------------------------------------------------
  # United States (5-digit ZIP)
  loc("United States", "New York", "New York", c("10001", "10016", "10029")),
  loc(
    "United States",
    "California",
    "Los Angeles",
    c("90001", "90012", "90095")
  ),
  loc("United States", "California", "San Francisco", c("94102", "94143")),
  loc("United States", "California", "San Diego", c("92101", "92103")),
  loc("United States", "Illinois", "Chicago", c("60601", "60614", "60637")),
  loc("United States", "Texas", "Houston", c("77002", "77030", "77054")),
  loc("United States", "Massachusetts", "Boston", c("02108", "02115", "02215")),
  loc("United States", "Washington", "Seattle", c("98101", "98104", "98195")),
  loc("United States", "Georgia", "Atlanta", c("30303", "30322", "30329")),
  loc("United States", "Florida", "Miami", c("33101", "33136")),
  loc("United States", "Colorado", "Denver", c("80202", "80204")),
  loc("United States", "Pennsylvania", "Philadelphia", c("19102", "19104")),
  loc("United States", "Minnesota", "Minneapolis", c("55401", "55455")),
  loc("United States", "Arizona", "Phoenix", c("85001", "85004")),
  loc("United States", "Tennessee", "Nashville", c("37203", "37232")),
  # Canada (alphanumeric)
  loc("Canada", "Ontario", "Toronto", c("M5H 2N2", "M5S 1A1")),
  loc("Canada", "Ontario", "Ottawa", c("K1P 1J1", "K1N 6N5")),
  loc("Canada", "Quebec", "Montreal", c("H2Y 1C6", "H3A 0G4")),
  loc("Canada", "Quebec", "Quebec City", c("G1R 2L3", "G1V 0A6")),
  loc("Canada", "British Columbia", "Vancouver", c("V6B 1A1", "V6T 1Z4")),
  loc("Canada", "Alberta", "Calgary", c("T2P 1J9", "T2N 1N4")),
  loc("Canada", "Alberta", "Edmonton", c("T5J 0N3", "T6G 2R3")),
  # Mexico (5-digit)
  loc("Mexico", "Mexico City", "Mexico City", c("06000", "06700")),
  loc("Mexico", "Jalisco", "Guadalajara", c("44100", "44600")),
  loc("Mexico", "Nuevo Leon", "Monterrey", c("64000", "64460")),
  loc("Mexico", "Puebla", "Puebla", c("72000", "72410")),
  # Brazil (XXXXX-XXX)
  loc("Brazil", "Sao Paulo", "Sao Paulo", c("01000-000", "05403-000")),
  loc(
    "Brazil",
    "Rio de Janeiro",
    "Rio de Janeiro",
    c("20010-000", "22250-040")
  ),
  loc("Brazil", "Federal District", "Brasilia", c("70040-010", "70297-400")),
  loc("Brazil", "Bahia", "Salvador", c("40010-000", "40170-110")),
  loc("Brazil", "Minas Gerais", "Belo Horizonte", c("30110-000", "30140-071")),
  loc(
    "Brazil",
    "Rio Grande do Sul",
    "Porto Alegre",
    c("90010-000", "90035-003")
  ),
  loc("Brazil", "Pernambuco", "Recife", c("50010-000", "50070-000")),
  loc("Brazil", "Parana", "Curitiba", c("80010-000", "80060-000")),
  # Argentina / Chile / Colombia
  loc("Argentina", "Buenos Aires", "Buenos Aires", c("1001", "1425")),
  loc("Argentina", "Cordoba", "Cordoba", c("5000", "5008")),
  loc("Argentina", "Santa Fe", "Rosario", c("2000", "2013")),
  loc("Argentina", "Mendoza", "Mendoza", c("5500", "5521")),
  loc(
    "Chile",
    "Santiago Metropolitan Region",
    "Santiago",
    c("8320000", "8330000")
  ),
  loc("Chile", "Valparaiso Region", "Valparaiso", c("2340000", "2360000")),
  loc("Chile", "Biobio Region", "Concepcion", c("4030000", "4070000")),
  loc("Colombia", "Bogota", "Bogota", c("110111", "110221")),
  loc("Colombia", "Antioquia", "Medellin", c("050001", "050010")),
  loc("Colombia", "Valle del Cauca", "Cali", c("760001", "760042")),

  # --- Asia ----------------------------------------------------------------
  # Japan (XXX-XXXX)
  loc("Japan", "Tokyo", "Tokyo", c("100-0001", "113-8654")),
  loc("Japan", "Osaka", "Osaka", c("530-0001", "565-0871")),
  loc("Japan", "Kanagawa", "Yokohama", c("220-0011", "231-0023")),
  loc("Japan", "Aichi", "Nagoya", c("460-0001", "464-0819")),
  loc("Japan", "Hokkaido", "Sapporo", c("060-0001", "060-0808")),
  loc("Japan", "Fukuoka", "Fukuoka", c("810-0001", "812-0011")),
  loc("Japan", "Kyoto", "Kyoto", c("600-8001", "606-8501")),
  # China (6-digit)
  loc("China", "Shanghai", "Shanghai", c("200001", "200032")),
  loc("China", "Beijing", "Beijing", c("100000", "100730")),
  loc("China", "Guangdong", "Guangzhou", c("510000", "510080")),
  loc("China", "Guangdong", "Shenzhen", c("518000", "518057")),
  loc("China", "Sichuan", "Chengdu", c("610000", "610041")),
  loc("China", "Hubei", "Wuhan", c("430000", "430030")),
  loc("China", "Shaanxi", "Xian", c("710000", "710061")),
  # India (6-digit)
  loc("India", "Maharashtra", "Mumbai", c("400001", "400012")),
  loc("India", "Delhi", "New Delhi", c("110001", "110029")),
  loc("India", "Karnataka", "Bengaluru", c("560001", "560034")),
  loc("India", "Tamil Nadu", "Chennai", c("600001", "600113")),
  loc("India", "West Bengal", "Kolkata", c("700001", "700020")),
  loc("India", "Telangana", "Hyderabad", c("500001", "500034")),
  loc("India", "Maharashtra", "Pune", c("411001", "411007")),
  loc("India", "Gujarat", "Ahmedabad", c("380001", "380015")),
  # South Korea / Thailand / Indonesia / Singapore (5-6 digit)
  loc("South Korea", "Seoul", "Seoul", c("04524", "03080")),
  loc("South Korea", "Busan", "Busan", c("48058", "49241")),
  loc("South Korea", "Incheon", "Incheon", c("21999", "22332")),
  loc("Thailand", "Bangkok", "Bangkok", c("10100", "10330")),
  loc("Thailand", "Chiang Mai", "Chiang Mai", c("50000", "50200")),
  loc("Thailand", "Phuket", "Phuket", c("83000", "83100")),
  loc("Indonesia", "Jakarta", "Jakarta", c("10110", "10430")),
  loc("Indonesia", "East Java", "Surabaya", c("60111", "60271")),
  loc("Indonesia", "West Java", "Bandung", c("40111", "40132")),
  loc("Singapore", "Central Region", "Singapore", c("018956", "138632")),
  # Israel / Saudi Arabia
  loc("Israel", "Jerusalem District", "Jerusalem", c("9100000", "9548301")),
  loc("Israel", "Tel Aviv District", "Tel Aviv", c("6100000", "6473424")),
  loc("Israel", "Haifa District", "Haifa", c("3100000", "3498838")),
  loc("Saudi Arabia", "Riyadh Province", "Riyadh", c("11564", "12211")),
  loc("Saudi Arabia", "Makkah Province", "Jeddah", c("21589", "23442")),
  loc("Saudi Arabia", "Eastern Province", "Dammam", c("31411", "32241")),

  # --- Oceania -------------------------------------------------------------
  # Australia (4-digit)
  loc("Australia", "New South Wales", "Sydney", c("2000", "2010")),
  loc("Australia", "Victoria", "Melbourne", c("3000", "3053")),
  loc("Australia", "Queensland", "Brisbane", c("4000", "4006")),
  loc("Australia", "Western Australia", "Perth", c("6000", "6009")),
  loc("Australia", "South Australia", "Adelaide", c("5000", "5006")),
  loc(
    "Australia",
    "Australian Capital Territory",
    "Canberra",
    c("2600", "2601")
  ),
  loc("Australia", "Tasmania", "Hobart", c("7000", "7005")),
  # New Zealand (4-digit)
  loc("New Zealand", "Auckland", "Auckland", c("1010", "1023")),
  loc("New Zealand", "Wellington", "Wellington", c("6011", "6021")),
  loc("New Zealand", "Canterbury", "Christchurch", c("8011", "8013")),

  # --- Africa --------------------------------------------------------------
  # South Africa (4-digit)
  loc("South Africa", "Gauteng", "Johannesburg", c("2000", "2193")),
  loc("South Africa", "Gauteng", "Pretoria", c("0002", "0181")),
  loc("South Africa", "Western Cape", "Cape Town", c("8000", "7925")),
  loc("South Africa", "KwaZulu-Natal", "Durban", c("4001", "4091")),
  loc("South Africa", "Eastern Cape", "Port Elizabeth", c("6001", "6070")),
  loc("South Africa", "Free State", "Bloemfontein", c("9301", "9320")),
  # Egypt / Nigeria / Kenya
  loc("Egypt", "Cairo Governorate", "Cairo", c("11511", "11765")),
  loc("Egypt", "Alexandria Governorate", "Alexandria", c("21500", "21599")),
  loc("Egypt", "Giza Governorate", "Giza", c("12511", "12611")),
  loc("Nigeria", "Lagos State", "Lagos", c("100001", "101241")),
  loc("Nigeria", "Federal Capital Territory", "Abuja", c("900001", "900108")),
  loc("Nigeria", "Kano State", "Kano", c("700001", "700213")),
  loc("Kenya", "Nairobi County", "Nairobi", c("00100", "00200")),
  loc("Kenya", "Mombasa County", "Mombasa", c("80100", "80200")),
  loc("Kenya", "Kisumu County", "Kisumu", c("40100", "40123"))
)

# Continent per country, so `enrich_metadata(continent = ...)` can restrict the
# spatial pool to one continent. (Transcontinental Turkey is filed under Europe,
# matching how it's grouped in location_table above.)
country_continent <- c(
  Germany = "Europe",
  `United Kingdom` = "Europe",
  France = "Europe",
  Spain = "Europe",
  Italy = "Europe",
  Netherlands = "Europe",
  Belgium = "Europe",
  Switzerland = "Europe",
  Austria = "Europe",
  Sweden = "Europe",
  Denmark = "Europe",
  Norway = "Europe",
  Finland = "Europe",
  Poland = "Europe",
  Portugal = "Europe",
  Ireland = "Europe",
  Greece = "Europe",
  `Czech Republic` = "Europe",
  Turkey = "Europe",
  `United States` = "Americas",
  Canada = "Americas",
  Mexico = "Americas",
  Brazil = "Americas",
  Argentina = "Americas",
  Chile = "Americas",
  Colombia = "Americas",
  Japan = "Asia",
  China = "Asia",
  India = "Asia",
  `South Korea` = "Asia",
  Thailand = "Asia",
  Indonesia = "Asia",
  Singapore = "Asia",
  Israel = "Asia",
  `Saudi Arabia` = "Asia",
  Australia = "Oceania",
  `New Zealand` = "Oceania",
  `South Africa` = "Africa",
  Egypt = "Africa",
  Nigeria = "Africa",
  Kenya = "Africa"
)
location_table$continent <- unname(
  country_continent[location_table$geo_loc_name_country]
)

# City centres (WGS84 decimal degrees) for `coordinates = TRUE`, one per city in
# location_table. City names are unique there, so the city alone is the key;
# every centre was checked to lie inside its country's Natural Earth outline,
# which the map's out-of-country advisory tests against.
city_centres <- read.csv(
  text = "city,lat,lon
Berlin,52.520,13.405
Munich,48.137,11.575
Nuremberg,49.452,11.077
Hamburg,53.551,9.994
Cologne,50.938,6.960
Dusseldorf,51.227,6.774
Frankfurt,50.110,8.682
Dresden,51.050,13.738
Leipzig,51.340,12.375
Stuttgart,48.776,9.183
Hanover,52.375,9.732
London,51.507,-0.128
Manchester,53.481,-2.243
Birmingham,52.486,-1.890
Leeds,53.801,-1.549
Liverpool,53.408,-2.992
Bristol,51.455,-2.588
Newcastle upon Tyne,54.978,-1.618
Sheffield,53.381,-1.470
Edinburgh,55.953,-3.188
Glasgow,55.864,-4.252
Aberdeen,57.150,-2.094
Cardiff,51.482,-3.179
Swansea,51.621,-3.944
Belfast,54.597,-5.930
Paris,48.857,2.352
Lyon,45.764,4.836
Marseille,43.297,5.370
Nice,43.710,7.262
Toulouse,43.605,1.444
Bordeaux,44.838,-0.579
Lille,50.629,3.057
Nantes,47.218,-1.554
Strasbourg,48.573,7.752
Madrid,40.417,-3.704
Barcelona,41.385,2.173
Valencia,39.470,-0.376
Seville,37.389,-5.984
Malaga,36.721,-4.421
Bilbao,43.263,-2.935
Zaragoza,41.649,-0.889
Rome,41.903,12.496
Milan,45.464,9.190
Naples,40.852,14.268
Turin,45.070,7.687
Bologna,44.494,11.343
Florence,43.770,11.256
Palermo,38.116,13.361
Genoa,44.405,8.946
Amsterdam,52.368,4.904
Utrecht,52.091,5.122
Rotterdam,51.924,4.478
The Hague,52.070,4.300
Eindhoven,51.441,5.470
Groningen,53.219,6.567
Brussels,50.850,4.352
Antwerp,51.219,4.402
Ghent,51.054,3.725
Bruges,51.209,3.225
Liege,50.633,5.567
Zurich,47.377,8.541
Geneva,46.204,6.143
Basel,47.560,7.589
Bern,46.948,7.447
Lausanne,46.520,6.633
Lucerne,47.050,8.309
Vienna,48.208,16.373
Graz,47.071,15.440
Salzburg,47.810,13.055
Innsbruck,47.269,11.404
Linz,48.306,14.286
Stockholm,59.329,18.069
Gothenburg,57.709,11.975
Malmo,55.605,13.004
Uppsala,59.859,17.639
Copenhagen,55.676,12.568
Aarhus,56.163,10.204
Odense,55.404,10.402
Oslo,59.914,10.752
Bergen,60.391,5.322
Trondheim,63.431,10.395
Helsinki,60.170,24.938
Tampere,61.498,23.761
Turku,60.452,22.267
Warsaw,52.230,21.012
Krakow,50.065,19.945
Wroclaw,51.108,17.039
Gdansk,54.352,18.646
Poznan,52.406,16.925
Lisbon,38.722,-9.139
Porto,41.158,-8.629
Coimbra,40.203,-8.410
Braga,41.545,-8.427
Dublin,53.350,-6.260
Cork,51.898,-8.471
Galway,53.271,-9.057
Limerick,52.664,-8.623
Athens,37.984,23.728
Thessaloniki,40.640,22.944
Patras,38.246,21.735
Prague,50.076,14.438
Brno,49.195,16.608
Ostrava,49.820,18.262
Istanbul,41.008,28.978
Ankara,39.934,32.860
Izmir,38.424,27.143
Antalya,36.897,30.713
New York,40.713,-74.006
Los Angeles,34.052,-118.244
San Francisco,37.775,-122.419
San Diego,32.716,-117.161
Chicago,41.878,-87.630
Houston,29.760,-95.370
Boston,42.360,-71.059
Seattle,47.606,-122.332
Atlanta,33.749,-84.388
Miami,25.762,-80.192
Denver,39.739,-104.990
Philadelphia,39.953,-75.165
Minneapolis,44.978,-93.265
Phoenix,33.448,-112.074
Nashville,36.163,-86.781
Toronto,43.653,-79.383
Ottawa,45.421,-75.697
Montreal,45.502,-73.567
Quebec City,46.813,-71.208
Vancouver,49.283,-123.121
Calgary,51.045,-114.072
Edmonton,53.546,-113.494
Mexico City,19.433,-99.133
Guadalajara,20.660,-103.350
Monterrey,25.686,-100.316
Puebla,19.041,-98.206
Sao Paulo,-23.551,-46.633
Rio de Janeiro,-22.907,-43.173
Brasilia,-15.794,-47.882
Salvador,-12.978,-38.501
Belo Horizonte,-19.917,-43.935
Porto Alegre,-30.035,-51.218
Recife,-8.048,-34.877
Curitiba,-25.429,-49.271
Buenos Aires,-34.604,-58.382
Cordoba,-31.420,-64.189
Rosario,-32.947,-60.639
Mendoza,-32.890,-68.845
Santiago,-33.449,-70.669
Valparaiso,-33.047,-71.613
Concepcion,-36.827,-73.050
Bogota,4.711,-74.072
Medellin,6.244,-75.581
Cali,3.452,-76.532
Tokyo,35.676,139.650
Osaka,34.694,135.502
Yokohama,35.444,139.638
Nagoya,35.181,136.906
Sapporo,43.062,141.354
Fukuoka,33.590,130.402
Kyoto,35.012,135.768
Shanghai,31.230,121.474
Beijing,39.904,116.407
Guangzhou,23.129,113.264
Shenzhen,22.543,114.058
Chengdu,30.573,104.066
Wuhan,30.593,114.305
Xian,34.341,108.940
Mumbai,19.076,72.878
New Delhi,28.614,77.209
Bengaluru,12.972,77.595
Chennai,13.083,80.271
Kolkata,22.573,88.364
Hyderabad,17.385,78.487
Pune,18.520,73.857
Ahmedabad,23.023,72.571
Seoul,37.567,126.978
Busan,35.180,129.076
Incheon,37.456,126.705
Bangkok,13.756,100.502
Chiang Mai,18.788,98.985
Phuket,7.880,98.392
Jakarta,-6.208,106.846
Surabaya,-7.250,112.769
Bandung,-6.917,107.619
Singapore,1.352,103.820
Jerusalem,31.769,35.216
Tel Aviv,32.085,34.782
Haifa,32.794,34.990
Riyadh,24.713,46.675
Jeddah,21.485,39.193
Dammam,26.392,49.978
Sydney,-33.869,151.209
Melbourne,-37.814,144.963
Brisbane,-27.470,153.026
Perth,-31.950,115.860
Adelaide,-34.929,138.601
Canberra,-35.281,149.130
Hobart,-42.882,147.327
Auckland,-36.849,174.763
Wellington,-41.287,174.776
Christchurch,-43.532,172.637
Johannesburg,-26.204,28.047
Pretoria,-25.747,28.229
Cape Town,-33.925,18.424
Durban,-29.858,31.022
Port Elizabeth,-33.962,25.602
Bloemfontein,-29.085,26.160
Cairo,30.044,31.236
Alexandria,31.200,29.919
Giza,30.013,31.209
Lagos,6.524,3.379
Abuja,9.077,7.399
Kano,12.002,8.592
Nairobi,-1.286,36.817
Mombasa,-4.043,39.668
Kisumu,-0.092,34.768",
  stringsAsFactors = FALSE
)

# Spellings the app may have rewritten a country to on load (app/logic/geo.R),
# mapped back to this table's names, so a re-run still finds the city.
country_aliases <- c(
  "czechia" = "czech republic",
  "united states of america" = "united states"
)

# Clinical / environmental specimen types, weighted towards the body sites most
# associated with Pseudomonas (respiratory, wound, urinary) while keeping a long
# diverse tail. `prob` is relative; sample() normalises it.
specimen_sources <- c(
  "Sputum",
  "Bronchoalveolar lavage",
  "Endotracheal aspirate",
  "Blood culture",
  "Wound swab",
  "Urine",
  "Pleural fluid",
  "Catheter tip",
  "Rectal swab",
  "Cerebrospinal fluid",
  "Tissue biopsy",
  "Ear swab",
  "Nasopharyngeal swab",
  "Burn wound swab",
  "Environmental surface swab",
  "Water sample"
)
specimen_weights <- c(8, 5, 4, 6, 7, 6, 2, 4, 3, 1, 3, 2, 2, 3, 2, 1)

purpose_of_sampling <- c(
  "Clinical diagnosis",
  "Routine surveillance",
  "Outbreak investigation",
  "Infection control screening",
  "Research study",
  "Environmental monitoring",
  "Antimicrobial resistance surveillance"
)

purpose_of_sequencing <- c(
  "Whole genome characterization",
  "Outbreak investigation",
  "Antimicrobial resistance profiling",
  "Phylogenetic analysis",
  "Species and strain confirmation",
  "Genomic surveillance",
  "Virulence gene detection"
)

# Sequencing is routinely outsourced, so the submitting centre need not be
# congruent with the sampling location — a global pool is realistic.
sequencing_centres <- c(
  "Wellcome Sanger Institute",
  "Broad Institute of MIT and Harvard",
  "BGI Genomics",
  "EMBL-EBI",
  "J. Craig Venter Institute",
  "Institut Pasteur",
  "Robert Koch Institute",
  "UK Health Security Agency",
  "US Centers for Disease Control and Prevention",
  "Statens Serum Institut",
  "National Institute of Infectious Diseases",
  "Wellcome Centre for Human Genetics"
)

# Collecting-institution name templates. {city} / {country} are substituted per
# row from the isolate's assigned location, so the collector is congruent with
# where the sample was taken.
collector_templates <- c(
  "{city} University Hospital",
  "{city} General Hospital",
  "{city} Public Health Laboratory",
  "{city} Institute of Medical Microbiology",
  "Department of Clinical Microbiology, {city}",
  "National Reference Laboratory, {country}"
)

# Centre of each row's city, or NA where the city is not in location_table or
# the row's country disagrees with the table's country for that city.
city_centre <- function(country, city) {
  key <- function(x) {
    x <- tolower(trimws(as.character(x)))
    ifelse(x %in% names(country_aliases), country_aliases[x], x)
  }
  table_country <- location_table$geo_loc_name_country[
    match(city_centres$city, location_table$geo_loc_name_city)
  ]
  i <- match(tolower(trimws(as.character(city))), tolower(city_centres$city))
  ok <- !is.na(i) & key(country) == key(table_country[i])
  i[!ok] <- NA_integer_
  data.frame(lat = city_centres$lat[i], lon = city_centres$lon[i])
}

# A uniformly random point within `km` of each centre, so isolates of one city
# spread over it instead of stacking on a single spot.
jitter_points <- function(lat, lon, km = 3) {
  d <- km * sqrt(runif(length(lat)))
  theta <- runif(length(lat), 0, 2 * pi)
  data.frame(
    lat = lat + d / 111.32 * cos(theta),
    lon = lon + d / (111.32 * cos(lat * pi / 180)) * sin(theta)
  )
}

# "lat, lon" at 5 decimals (~1 m) without trailing zeros, the form the app
# stores coordinates in.
format_coords <- function(lat, lon) {
  num <- function(x) sub("\\.?0+$", "", sprintf("%.5f", x))
  paste0(num(lat), ", ", num(lon))
}

# --- main entry point ------------------------------------------------------

#' Enrich a database's metadata table with artificial example data.
#'
#' @param db_path     Path to a PhyloTrace SQLite database.
#' @param overwrite   If FALSE (default) only empty cells are filled and
#'                    existing values preserved; if TRUE every enrichable cell
#'                    is regenerated.
#' @param seed        Optional integer for reproducible output.
#' @param continent   Optional single continent name to restrict the spatial
#'                    pool to (case-insensitive), e.g. "Europe". One of
#'                    Africa, Americas, Asia, Europe, Oceania. NULL (default)
#'                    draws from all continents.
#' @param date_range  Length-2 character vector (YYYY-MM-DD) bounding the
#'                    randomly drawn collection dates.
#' @param preserve    Columns never touched (default isolate + organism).
#' @param coordinates If TRUE, also fill geo_loc_coordinates with a point within
#'                    3 km of each isolate's city centre. Only rows whose city is
#'                    in location_table get one.
#' @param coord_missing Share of those rows left without coordinates, so the
#'                    map's geocoding still has places to look up.
#' @param bad_geo     Number of rows (among those given coordinates) to corrupt
#'                    on purpose, to exercise the map's data advisories. Cycles
#'                    through swapped coordinates, coordinates in another
#'                    country, unreadable coordinates and an unknown country.
#' @return The updated metadata data.frame, invisibly.
#' @export
enrich_metadata <- function(
  db_path,
  overwrite = FALSE,
  seed = NULL,
  continent = NULL,
  date_range = c("2015-01-01", "2024-12-31"),
  preserve = c("isolate", "organism"),
  coordinates = FALSE,
  coord_missing = 0.1,
  bad_geo = 0L
) {
  if (bad_geo > 0 && !coordinates) {
    stop("`bad_geo` needs `coordinates = TRUE`.")
  }
  if (!file.exists(db_path)) {
    stop("Database not found: ", db_path)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Restrict the spatial pool to one continent if asked; congruence is
  # unaffected since every field still comes from the same location row.
  locations <- location_table
  if (!is.null(continent)) {
    if (length(continent) != 1L || is.na(continent)) {
      stop("`continent` must be a single continent name, or NULL.")
    }
    choices <- sort(unique(location_table$continent))
    hit <- match(tolower(continent), tolower(choices))
    if (is.na(hit)) {
      stop(
        "Unknown continent '",
        continent,
        "'. Choose one of: ",
        paste(choices, collapse = ", ")
      )
    }
    locations <- location_table[
      location_table$continent == choices[hit],
      ,
      drop = FALSE
    ]
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  if (!"metadata" %in% dbListTables(con)) {
    stop("No `metadata` table in ", db_path)
  }
  meta <- dbReadTable(con, "metadata")
  n <- nrow(meta)
  if (n == 0L) {
    stop("`metadata` table has no rows to enrich.")
  }

  is_empty <- function(x) {
    x <- as.character(x)
    is.na(x) | !nzchar(trimws(x))
  }

  geo_cols <- c(
    "geo_loc_name_country",
    "geo_loc_name_state_province",
    "geo_loc_name_city",
    "geo_loc_name_postal_code"
  )

  # Coerce every enrichable column that exists to character so partially-NULL
  # columns (read back as logical) accept string assignment cleanly.
  enrichable <- setdiff(
    c(
      geo_cols,
      "sample_collection_date",
      "primary_laboratory_sample_id",
      "specimen_source_id",
      "sample_collected_by",
      "sequence_submitted_by",
      "purpose_of_sampling",
      "purpose_of_sequencing"
    ),
    preserve
  )
  for (col in intersect(enrichable, names(meta))) {
    meta[[col]] <- as.character(meta[[col]])
  }

  # Fill a single column, honouring overwrite + column presence.
  fill_col <- function(col_name, values) {
    if (!col_name %in% names(meta)) {
      return(invisible())
    }
    target <- if (overwrite) rep(TRUE, n) else is_empty(meta[[col_name]])
    meta[[col_name]][target] <<- values[target]
    invisible()
  }

  # --- spatial block (assigned as a unit to stay congruent) ----------------
  present_geo <- setdiff(intersect(geo_cols, names(meta)), preserve)
  # Rows whose location this run generated; only these may get an invented
  # country below, so an existing value is never replaced by a broken one.
  relocated <- rep(overwrite, n)
  if (length(present_geo)) {
    # A row is (re)located only when ALL its geo cells are empty, or when
    # overwrite = TRUE — never merge a fresh city into a row that already holds
    # some geo data, or congruence would break.
    geo_empty <- if (overwrite) {
      rep(TRUE, n)
    } else {
      Reduce(`&`, lapply(present_geo, function(c) is_empty(meta[[c]])))
    }
    relocated <- geo_empty
    idx <- sample(nrow(locations), n, replace = TRUE)
    for (c in present_geo) {
      meta[[c]][geo_empty] <- locations[[c]][idx][geo_empty]
    }

    # A row that carries only its country (the usual shape of a public
    # dataset) is refined *within* that country: its missing state, city and
    # postcode are drawn from the same country's locations, so the recorded
    # country is kept and the new fields stay congruent with it. Countries
    # absent from the location table are left as they are.
    finer <- setdiff(present_geo, "geo_loc_name_country")
    if (!overwrite && "geo_loc_name_country" %in% present_geo && length(finer)) {
      country <- as.character(meta[["geo_loc_name_country"]])
      anchored <- !geo_empty &
        !is_empty(country) &
        Reduce(`&`, lapply(finer, function(c) is_empty(meta[[c]]))) &
        country %in% locations$geo_loc_name_country
      for (ctry in unique(country[anchored])) {
        rows <- which(anchored & country == ctry)
        pool <- which(locations$geo_loc_name_country == ctry)
        pick <- pool[sample.int(length(pool), length(rows), replace = TRUE)]
        for (c in finer) {
          meta[[c]][rows] <- locations[[c]][pick]
        }
      }
    }
  }

  # --- collection dates ----------------------------------------------------
  days <- seq(as.Date(date_range[1]), as.Date(date_range[2]), by = "day")
  fill_col(
    "sample_collection_date",
    format(sample(days, n, replace = TRUE), "%Y-%m-%d")
  )

  # --- lab sample id (year tied to the collection date where available) ----
  yr <- substr(as.character(meta[["sample_collection_date"]]), 1, 4)
  blank_yr <- is_empty(yr)
  if (any(blank_yr)) {
    yr[blank_yr] <- format(sample(days, sum(blank_yr), replace = TRUE), "%Y")
  }
  lab_id <- paste0(
    sample(c("LAB", "MB", "CLN", "PHL", "REF", "ISO"), n, replace = TRUE),
    "-",
    yr,
    "-",
    sprintf("%05d", sample.int(99999L, n, replace = TRUE))
  )
  fill_col("primary_laboratory_sample_id", lab_id)

  # --- collecting institution (congruent with the isolate's location) ------
  city_now <- if ("geo_loc_name_city" %in% names(meta)) {
    meta[["geo_loc_name_city"]]
  } else {
    rep(NA_character_, n)
  }
  country_now <- if ("geo_loc_name_country" %in% names(meta)) {
    meta[["geo_loc_name_country"]]
  } else {
    rep(NA_character_, n)
  }
  collected_by <- vapply(
    seq_len(n),
    function(i) {
      city <- city_now[i]
      country <- country_now[i]
      # Fall back gracefully when a row has no place to anchor the name to.
      pool <- collector_templates
      if (is_empty(city)) {
        pool <- pool[!grepl("\\{city\\}", pool)]
      }
      if (is_empty(country)) {
        pool <- pool[!grepl("\\{country\\}", pool)]
      }
      if (!length(pool)) {
        return("National Reference Laboratory")
      }
      tmpl <- if (length(pool) == 1L) pool else sample(pool, 1)
      tmpl <- gsub(
        "{city}",
        if (is_empty(city)) "" else city,
        tmpl,
        fixed = TRUE
      )
      tmpl <- gsub(
        "{country}",
        if (is_empty(country)) "" else country,
        tmpl,
        fixed = TRUE
      )
      trimws(tmpl)
    },
    character(1)
  )
  fill_col("sample_collected_by", collected_by)

  # --- remaining categorical fields ----------------------------------------
  fill_col(
    "specimen_source_id",
    sample(specimen_sources, n, replace = TRUE, prob = specimen_weights)
  )
  fill_col(
    "sequence_submitted_by",
    sample(sequencing_centres, n, replace = TRUE)
  )
  fill_col(
    "purpose_of_sampling",
    sample(purpose_of_sampling, n, replace = TRUE)
  )
  fill_col(
    "purpose_of_sequencing",
    sample(purpose_of_sequencing, n, replace = TRUE)
  )

  # --- coordinates (last, so the draws above match runs without them) ------
  coord_col <- "geo_loc_coordinates"
  bad_report <- character(0)
  if (coordinates && !coord_col %in% preserve) {
    if (!coord_col %in% names(meta)) {
      meta[[coord_col]] <- NA_character_
    }
    meta[[coord_col]] <- as.character(meta[[coord_col]])
    country_col <- if ("geo_loc_name_country" %in% names(meta)) {
      meta[["geo_loc_name_country"]]
    } else {
      rep(NA_character_, n)
    }
    city_col <- if ("geo_loc_name_city" %in% names(meta)) {
      meta[["geo_loc_name_city"]]
    } else {
      rep(NA_character_, n)
    }
    centre <- city_centre(country_col, city_col)
    open_cell <- if (overwrite) rep(TRUE, n) else is_empty(meta[[coord_col]])
    target <- open_cell & !is.na(centre$lat)
    skip <- target & runif(n) < coord_missing
    meta[[coord_col]][skip & overwrite] <- NA_character_
    target <- target & !skip
    pts <- jitter_points(centre$lat[target], centre$lon[target])
    meta[[coord_col]][target] <- format_coords(pts$lat, pts$lon)

    # Deliberately broken rows, one kind after the other.
    kinds <- c("swapped", "other country", "unreadable", "unknown country")
    pool <- which(target)
    picked <- pool[sample.int(length(pool), min(bad_geo, length(pool)))]
    for (k in seq_along(picked)) {
      i <- picked[k]
      kind <- kinds[(k - 1L) %% length(kinds) + 1L]
      if (kind == "unknown country" && !relocated[i]) {
        kind <- "unreadable"
      }
      if (kind == "swapped") {
        meta[[coord_col]][i] <- format_coords(pts$lon[pool == i], pts$lat[pool == i])
      } else if (kind == "other country") {
        far <- city_centres[
          location_table$geo_loc_name_country[
            match(city_centres$city, location_table$geo_loc_name_city)
          ] != country_col[i],
        ]
        far <- far[sample.int(nrow(far), 1), ]
        meta[[coord_col]][i] <- format_coords(far$lat, far$lon)
      } else if (kind == "unreadable") {
        meta[[coord_col]][i] <- sample(c("next to the hospital", "52.5", "N/A?"), 1)
      } else {
        meta[["geo_loc_name_country"]][i] <- sample(c("Atlantis", "Narnia", "Wakanda"), 1)
      }
      bad_report <- c(bad_report, sprintf("  %-16s %s", kind, meta[["isolate"]][i]))
    }
  }

  # --- write back ----------------------------------------------------------
  # Mirrors the app's own metadata update (app/logic/database_functions.R):
  # rewrite the whole table. metadata carries no indexes/constraints, so an
  # overwrite is lossless.
  dbWriteTable(con, "metadata", meta, overwrite = TRUE, row.names = FALSE)

  n_countries <- length(unique(meta[["geo_loc_name_country"]]))
  n_cities <- length(unique(meta[["geo_loc_name_city"]]))
  message(sprintf(
    "Enriched %d rows in %s (%d countries, %d cities represented).",
    n,
    basename(db_path),
    n_countries,
    n_cities
  ))
  if (coordinates && coord_col %in% names(meta)) {
    message(sprintf(
      "%d of %d rows now carry coordinates.",
      sum(!is_empty(meta[[coord_col]])),
      n
    ))
  }
  if (length(bad_report)) {
    message("Deliberately broken rows:\n", paste(bad_report, collapse = "\n"))
  }
  invisible(meta)
}

# --- CLI -------------------------------------------------------------------
# Runs only under Rscript, not when the file is source()d for the function.
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args)) {
    stop(
      paste(
        "Usage: Rscript dev/enrich_metadata.R <database.db> [--overwrite]",
        "[--seed N] [--continent NAME] [--coordinates] [--bad-geo N]"
      )
    )
  }
  db_path <- args[[1]]
  overwrite <- "--overwrite" %in% args
  seed <- NULL
  if ("--seed" %in% args) {
    seed <- as.integer(args[[which(args == "--seed") + 1L]])
  }
  continent <- NULL
  if ("--continent" %in% args) {
    continent <- args[[which(args == "--continent") + 1L]]
  }
  bad_geo <- 0L
  if ("--bad-geo" %in% args) {
    bad_geo <- as.integer(args[[which(args == "--bad-geo") + 1L]])
  }
  enrich_metadata(
    db_path,
    overwrite = overwrite,
    seed = seed,
    continent = continent,
    coordinates = "--coordinates" %in% args || bad_geo > 0,
    bad_geo = bad_geo
  )
}

if (identical(environment(), globalenv()) && sys.nframe() == 0L) {
  main()
}
