# library(tidygeocoder)
rm(list = ls())
gc()
poptrag <- readRDS("data-raw/poptrag.rds")

# Extract unique origins from your data
unique_origins <- poptrag$artist.mb.origin |>
  unique() |>
  na.omit()

origin_lookup <- tibble::tibble(
  original = unique_origins
)
# Geocode all locations
message(
  "Starting geocoding process. This may take 30-60 minutes for ",
  nrow(origin_lookup),
  " locations..."
)

origin_lookup_geocoded <- origin_lookup |>
  tidygeocoder::geocode(
    address = original,
    method = "osm",
    lat = latitude,
    long = longitude,
    full_results = TRUE,
    progress_bar = TRUE,
    verbose = TRUE
  )

# Extract country and continent from geocoding results
origin_lookup_final <- origin_lookup_geocoded |>
  dplyr::mutate(
    # Extract country from display_name (usually last component)
    country_raw = dplyr::if_else(
      !is.na(display_name),
      stringr::str_extract(display_name, "[^,]+$") |> stringr::str_trim(),
      original
    ),

    # Standardize country names
    country = dplyr::case_when(
      country_raw %in% c("Deutschland", "Allemagne") ~ "Germany",
      country_raw %in% c("Frankreich", "République française") ~ "France",
      country_raw %in% c("España") ~ "Spain",
      country_raw %in% c("Italia") ~ "Italy",
      country_raw %in% c("Nederland") ~ "Netherlands",
      country_raw %in% c("Österreich") ~ "Austria",
      country_raw %in% c("Schweiz", "Suisse", "Svizzera") ~ "Switzerland",
      country_raw %in% c("België", "Belgique") ~ "Belgium",
      country_raw %in% c("Sverige") ~ "Sweden",
      country_raw %in% c("Norge") ~ "Norway",
      country_raw %in% c("Danmark") ~ "Denmark",
      country_raw %in% c("Suomi") ~ "Finland",
      country_raw %in% c("Polska") ~ "Poland",
      country_raw %in% c("Česko") ~ "Czechia",
      country_raw %in% c("Magyarország") ~ "Hungary",
      country_raw %in% c("Ελλάδα", "Ελλάς") ~ "Greece",
      country_raw %in% c("Shqipëria") ~ "Albania",
      country_raw %in% c("Україна") ~ "Ukraine",
      country_raw %in% c("Россия") ~ "Russia",
      country_raw %in% c("México") ~ "Mexico",
      country_raw %in% c("Brasil") ~ "Brazil",
      country_raw %in% c("日本") ~ "Japan",
      country_raw %in% c("中国") ~ "China",
      country_raw %in% c("대한민국", "Republic of Korea") ~ "South Korea",
      country_raw %in% c("ไทย", "ประเทศไทย") ~ "Thailand",
      country_raw %in% c("Pilipinas") ~ "Philippines",
      country_raw %in% c("भारत", "Bhārat") ~ "India",
      country_raw %in% c("مصر") ~ "Egypt",
      country_raw %in% c("Maroc") ~ "Morocco",
      country_raw %in% c("República Dominicana") ~ "Dominican Republic",
      country_raw %in% c("Polynésie française") ~ "French Polynesia",
      country_raw %in% c("Aotearoa") ~ "New Zealand",
      country_raw %in% c("Haïti") ~ "Haiti",
      TRUE ~ country_raw
    ),

    # Assign continent
    continent = dplyr::case_when(
      country %in%
        c(
          "United Kingdom",
          "Germany",
          "France",
          "Spain",
          "Italy",
          "Netherlands",
          "Austria",
          "Switzerland",
          "Belgium",
          "Portugal",
          "Ireland",
          "Sweden",
          "Norway",
          "Denmark",
          "Finland",
          "Iceland",
          "Poland",
          "Czechia",
          "Czech Republic",
          "Slovakia",
          "Hungary",
          "Romania",
          "Bulgaria",
          "Greece",
          "Croatia",
          "Serbia",
          "Slovenia",
          "Bosnia and Herzegovina",
          "North Macedonia",
          "Albania",
          "Cyprus",
          "Malta",
          "Liechtenstein",
          "Luxembourg",
          "Monaco",
          "San Marino",
          "Ukraine",
          "Belarus",
          "Moldova",
          "Estonia",
          "Latvia",
          "Lithuania",
          "Russia",
          "England",
          "Scotland",
          "Wales",
          "Northern Ireland"
        ) ~ "Europe",

      country %in%
        c(
          "United States",
          "United States of America",
          "Canada",
          "Mexico",
          "Guatemala",
          "Belize",
          "El Salvador",
          "Honduras",
          "Nicaragua",
          "Costa Rica",
          "Panama"
        ) ~ "North America",

      country %in%
        c(
          "Brazil",
          "Argentina",
          "Chile",
          "Colombia",
          "Venezuela",
          "Peru",
          "Ecuador",
          "Bolivia",
          "Paraguay",
          "Uruguay",
          "Guyana",
          "Suriname",
          "French Guiana"
        ) ~ "South America",

      country %in%
        c(
          "Japan",
          "China",
          "South Korea",
          "Thailand",
          "Vietnam",
          "Indonesia",
          "Malaysia",
          "Philippines",
          "Singapore",
          "Myanmar",
          "Cambodia",
          "Laos",
          "India",
          "Pakistan",
          "Bangladesh",
          "Nepal",
          "Bhutan",
          "Sri Lanka",
          "Turkey",
          "Iran",
          "Iraq",
          "Saudi Arabia",
          "Israel",
          "Palestine",
          "Jordan",
          "Lebanon",
          "Syria",
          "United Arab Emirates",
          "Kuwait",
          "Qatar",
          "Bahrain",
          "Oman",
          "Yemen",
          "Afghanistan",
          "Kazakhstan",
          "Uzbekistan",
          "Turkmenistan",
          "Kyrgyzstan",
          "Tajikistan",
          "Mongolia",
          "North Korea",
          "Taiwan",
          "Hong Kong",
          "Macau"
        ) ~ "Asia",

      country %in%
        c(
          "South Africa",
          "Nigeria",
          "Kenya",
          "Ghana",
          "Tanzania",
          "Uganda",
          "Ethiopia",
          "Morocco",
          "Algeria",
          "Tunisia",
          "Libya",
          "Egypt",
          "Senegal",
          "Côte d'Ivoire",
          "Mali",
          "Niger",
          "Burkina Faso",
          "Cameroon",
          "Zimbabwe",
          "Mozambique",
          "Madagascar",
          "Angola",
          "Somalia",
          "Sudan",
          "Eritrea",
          "Botswana",
          "Namibia",
          "Zambia",
          "Malawi",
          "Rwanda",
          "Burundi",
          "Democratic Republic of the Congo"
        ) ~ "Africa",

      country %in%
        c(
          "Australia",
          "New Zealand",
          "Papua New Guinea",
          "Fiji",
          "Solomon Islands",
          "Vanuatu",
          "Samoa",
          "Tonga",
          "French Polynesia",
          "New Caledonia"
        ) ~ "Oceania",

      country %in%
        c(
          "Jamaica",
          "Cuba",
          "Haiti",
          "Dominican Republic",
          "Puerto Rico",
          "Trinidad and Tobago",
          "Bahamas",
          "Barbados",
          "Saint Lucia",
          "Grenada",
          "Saint Vincent and the Grenadines",
          "Antigua and Barbuda",
          "Dominica",
          "Saint Kitts and Nevis",
          "Aruba",
          "Curaçao",
          "Bermuda",
          "Guadeloupe",
          "Martinique",
          "U.S. Virgin Islands",
          "British Virgin Islands"
        ) ~ "North America",

      TRUE ~ NA_character_
    )
  ) |>
  dplyr::select(original, country, continent)

# Save the lookup table
saveRDS(origin_lookup_final, "data/artist_origin_lookup.rds")

# View summary
summary_stats <- origin_lookup_final |>
  dplyr::summarize(
    total_locations = dplyr::n(),
    countries_identified = sum(!is.na(country)),
    continents_identified = sum(!is.na(continent)),
    missing = sum(is.na(country))
  )

print(summary_stats)

# Show top countries
top_countries <- origin_lookup_final |>
  dplyr::count(country, continent, sort = TRUE) |>
  head(20)

message("\nTop 20 countries by number of origin locations:")
print(top_countries)

# Show locations that failed to map to a country
failed_mapping <- origin_lookup_final |>
  dplyr::filter(is.na(country)) |>
  dplyr::select(original)

if (nrow(failed_mapping) > 0) {
  message(
    "\nLocations without country mapping (",
    nrow(failed_mapping),
    " total):"
  )
  print(failed_mapping, n = 20)
}
