rm(list = ls())
gc()
save_checkpoint_and_count <- function(
  f,
  checkpoint_filename,
  last_index,
  checkpoint_data,
  savingstep = 1,
  ndatapoints = -1
) {
  force(f)
  force(checkpoint_filename)
  force(last_index)
  force(checkpoint_data)
  force(ndatapoints)

  if (savingstep > 1 & ndatapoints == -1) {
    stop(paste0(
      'Please provide the number of data points ndatapoints when using a saving stepsize > 1; used savingstep = ',
      savingstep,
      ' and ndatapoints = ',
      ndatapoints,
      '...'
    ))
  }
  i <- last_index
  ndatapoints <- last_index + ndatapoints
  old_filename <- suppressMessages(
    read_checkpoint(checkpoint_filename)$last_checkpoint
  )
  saved_data <- checkpoint_data

  function(...) {
    i <<- i + 1
    result <- f(...)
    saved_data <<- dplyr::bind_rows(saved_data, result)
    if (i %% savingstep == 0 | i == ndatapoints) {
      current_filename <- paste0(checkpoint_filename, '_', i, '.rds')
      saveRDS(saved_data, current_filename)
      if (file.exists(old_filename)) {
        file.remove(old_filename)
      }
      old_filename <<- current_filename
    }
    Sys.sleep(0.3)
    result
  }
}


read_checkpoint <- function(checkpointfilename) {
  pattern <- paste0('^', checkpointfilename, '_(\\d+)\\.rds$')
  files <- list.files()
  matching_files <- grep(pattern, files, value = TRUE)

  if (length(matching_files) > 0) {
    last_index <- sub(pattern, '\\1', matching_files) |> as.integer() |> max()
    message(paste0(
      'Detected checkpoint. Trying to continue with index ',
      last_index + 1,
      '...'
    ))
    last_checkpoint <- paste0(checkpointfilename, '_', last_index, '.rds')
    saved_data <- readRDS(last_checkpoint)
    return(list(
      last_index = last_index,
      saved_data = saved_data,
      last_checkpoint = last_checkpoint
    ))
  } else {
    return(list(last_index = 0, saved_data = c(), last_checkpoint = ''))
  }
}

lookup_original <- function(address) {
  address <- tibble::tibble(original = address)
  result <- suppressMessages(
    address |>
      tidygeocoder::geocode(
        address = original,
        method = "osm",
        lat = latitude,
        long = longitude,
        full_results = TRUE
      )
  )
  return(result)
}


# library(tidygeocoder)
poptrag <- readRDS("data-raw/poptrag.rds")

# Extract unique origins from your data
unique_origins <- poptrag$artist.mb.origin |>
  unique() |>
  na.omit()

# Geocode all locations
message(
  "Starting geocoding process. This may take 30-60 minutes for ",
  length(unique_origins),
  " locations..."
)

input <- unique_origins
checkpoint_name <- 'geocode_artist_origins'
checkpoint <- read_checkpoint(checkpoint_name)
last_index <- checkpoint$last_index
saved_data <- checkpoint$saved_data
if (last_index > 0) {
  input <- tail(input, -last_index)
}
purrr::pmap_df(
  list(input),
  lookup_original |>
    save_checkpoint_and_count(
      checkpoint_name,
      last_index,
      saved_data,
      300,
      length(unique_origins)
    ),
  .progress = 'Geocoding origins'
)
original_lookup_geocoded <- suppressMessages(
  read_checkpoint(checkpoint_name)$saved_data
)
message('Done.')


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
