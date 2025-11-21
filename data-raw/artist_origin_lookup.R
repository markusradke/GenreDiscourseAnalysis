rm(list = ls())
gc()
# save_checkpoint_and_count <- function(
#   f,
#   checkpoint_filename,
#   last_index,
#   checkpoint_data,
#   savingstep = 1,
#   ndatapoints = -1
# ) {
#   force(f)
#   force(checkpoint_filename)
#   force(last_index)
#   force(checkpoint_data)
#   force(ndatapoints)

#   if (savingstep > 1 & ndatapoints == -1) {
#     stop(paste0(
#       'Please provide the number of data points ndatapoints when using a saving stepsize > 1; used savingstep = ',
#       savingstep,
#       ' and ndatapoints = ',
#       ndatapoints,
#       '...'
#     ))
#   }
#   i <- last_index
#   ndatapoints <- last_index + ndatapoints
#   old_filename <- suppressMessages(
#     read_checkpoint(checkpoint_filename)$last_checkpoint
#   )
#   saved_data <- checkpoint_data

#   function(...) {
#     i <<- i + 1
#     result <- f(...)
#     saved_data <<- dplyr::bind_rows(saved_data, result)
#     if (i %% savingstep == 0 | i == ndatapoints) {
#       current_filename <- paste0(checkpoint_filename, '_', i, '.rds')
#       saveRDS(saved_data, current_filename)
#       if (file.exists(old_filename)) {
#         file.remove(old_filename)
#       }
#       old_filename <<- current_filename
#     }
#     Sys.sleep(0.3)
#     result
#   }
# }

# read_checkpoint <- function(checkpointfilename) {
#   pattern <- paste0('^', checkpointfilename, '_(\\d+)\\.rds$')
#   files <- list.files()
#   matching_files <- grep(pattern, files, value = TRUE)

#   if (length(matching_files) > 0) {
#     last_index <- sub(pattern, '\\1', matching_files) |> as.integer() |> max()
#     message(paste0(
#       'Detected checkpoint. Trying to continue with index ',
#       last_index + 1,
#       '...'
#     ))
#     last_checkpoint <- paste0(checkpointfilename, '_', last_index, '.rds')
#     saved_data <- readRDS(last_checkpoint)
#     return(list(
#       last_index = last_index,
#       saved_data = saved_data,
#       last_checkpoint = last_checkpoint
#     ))
#   } else {
#     return(list(last_index = 0, saved_data = c(), last_checkpoint = ''))
#   }
# }

# lookup_original <- function(address) {
#   address <- tibble::tibble(original = address)
#   result <- suppressMessages(
#     address |>
#       tidygeocoder::geocode(
#         address = original,
#         method = "osm",
#         lat = latitude,
#         long = longitude,
#         full_results = TRUE
#       )
#   )
#   return(result)
# }

# # library(tidygeocoder)
# poptrag <- readRDS("data-raw/poptrag.rds")

# # Extract unique origins from your data
# unique_origins <- poptrag$artist.mb.origin |>
#   unique() |>
#   na.omit()

# # Geocode all locations
# message(
#   "Starting geocoding process. This may take 30-60 minutes for ",
#   length(unique_origins),
#   " locations..."
# )

# input <- unique_origins
# checkpoint_name <- 'geocode_artist_origins'
# checkpoint <- read_checkpoint(checkpoint_name)
# last_index <- checkpoint$last_index
# saved_data <- checkpoint$saved_data
# if (last_index > 0) {
#   input <- tail(input, -last_index)
# }
# res <- purrr::pmap_df(
#   list(input),
#   lookup_original |>
#     save_checkpoint_and_count(
#       checkpoint_name,
#       last_index,
#       saved_data,
#       300,
#       length(unique_origins)
#     ),
#   .progress = 'Geocoding origins'
# )
# origin_lookup_geocoded <- suppressMessages(
#   read_checkpoint(checkpoint_name)$saved_data
# )
# message('Done.')
# origin_lookup_geocoded <- rbind(origin_lookup_geocoded, res)
# saveRDS(origin_lookup_geocoded, "data-raw/artist_origin_geocoded_raw.rds")
origin_lookup_geocoded <- readRDS("data-raw/artist_origin_geocoded_raw.rds")

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
      # Additional country mappings
      country_raw %in% c("Slovensko") ~ "Slovakia",
      country_raw %in% c("Éire / Ireland") ~ "Ireland",
      country_raw %in% c("Suomi / Finland") ~ "Finland",
      country_raw %in% c("Slovenija") ~ "Slovenia",
      country_raw %in% c("România") ~ "Romania",
      country_raw %in% c("Schweiz/Suisse/Svizzera/Svizra") ~ "Switzerland",
      country_raw %in% c("ایران") ~ "Iran",
      country_raw %in% c("Ísland") ~ "Iceland",
      country_raw %in% c("Aotearoa / New Zealand") ~ "New Zealand",
      country_raw %in% c("België / Belgique / Belgien") ~ "Belgium",
      country_raw %in% c("ישראל") ~ "Israel",
      country_raw %in% c("Türkiye") ~ "Turkey",
      country_raw %in% c("Eesti") ~ "Estonia",
      country_raw %in% c("الكويت") ~ "Kuwait",
      country_raw %in% c("لبنان") ~ "Lebanon",
      country_raw %in% c("Беларусь") ~ "Belarus",
      country_raw %in% c("تونس") ~ "Tunisia",
      country_raw %in% c("Kosova / Kosovo") ~ "Kosovo",
      country_raw %in% c("Việt Nam") ~ "Vietnam",
      country_raw %in% c("Lietuva") ~ "Lithuania",
      country_raw %in%
        c(
          "République démocratique du Congo"
        ) ~ "Democratic Republic of the Congo",
      country_raw %in% c("ኤርትራ Eritrea إرتريا") ~ "Eritrea",
      country_raw %in% c("Қазақстан") ~ "Kazakhstan",
      country_raw %in% c("Føroyar") ~ "Faroe Islands",
      country_raw %in% c("ليبيا") ~ "Libya",
      country_raw %in% c("پاکستان") ~ "Pakistan",
      country_raw %in% c("Србија") ~ "Serbia",
      country_raw %in% c("საქართველო") ~ "Georgia",
      country_raw %in% c("नेपाल") ~ "Nepal",
      country_raw %in%
        c(
          "Bosna i Hercegovina / Босна и Херцеговина"
        ) ~ "Bosnia and Herzegovina",
      country_raw %in% c("Maroc ⵍⵎⵖⵔⵉⴱ المغرب") ~ "Morocco",
      country_raw %in% c("الإمارات العربية المتحدة") ~ "United Arab Emirates",
      country_raw %in% c("Kalaallit Nunaat") ~ "Greenland",
      country_raw %in% c("България") ~ "Bulgaria",
      country_raw %in% c("Panamá") ~ "Panama",
      country_raw %in% c("The Bahamas") ~ "Bahamas",
      country_raw %in% c("Ayiti") ~ "Haiti",
      country_raw %in% c("Sénégal") ~ "Senegal",
      country_raw %in% c("الأردن") ~ "Jordan",
      country_raw %in% c("臺灣") ~ "Taiwan",
      country_raw %in% c("Sierra Leone") ~ "Sierra Leone",
      country_raw %in% c("Северна Македонија") ~ "North Macedonia",
      country_raw %in% c("Latvija") ~ "Latvia",
      country_raw %in% c("Hrvatska") ~ "Croatia",
      country_raw %in% c("Bénin") ~ "Benin",
      country_raw %in% c("Kirovohrads'ka Oblast'") ~ "Ukraine",
      country_raw %in% c("Монгол улс ᠮᠤᠩᠭᠤᠯ ᠤᠯᠤᠰ") ~ "Mongolia",
      country_raw %in% c("البحرين") ~ "Bahrain",
      country_raw %in% c("বাংলাদেশ") ~ "Bangladesh",
      country_raw %in% c("Perú") ~ "Peru",
      country_raw %in% c("Algérie ⵍⵣⵣⴰⵢⴻⵔ الجزائر") ~ "Algeria",
      country_raw %in% c("افغانستان") ~ "Afghanistan",
      country_raw %in% c("Waitsfield (CDP)") ~ "United States",
      country_raw %in% c("Gambia") ~ "Gambia",
      country_raw %in% c("Soomaaliya الصومال") ~ "Somalia",
      country_raw %in% c("འབྲུག་ཡུལ།") ~ "Bhutan",
      country_raw %in% c("Gabon") ~ "Gabon",
      country_raw %in% c("Тоҷикистон") ~ "Tajikistan",
      country_raw %in% c("Culiacán Municipality") ~ "Mexico",
      country_raw %in% c("Հայաստան") ~ "Armenia",
      country_raw %in% c("Κύπρος - Kıbrıs") ~ "Cyprus",
      country_raw %in% c("Madagasikara / Madagascar") ~ "Madagascar",
      country_raw %in% c("Moçambique") ~ "Mozambique",
      country_raw %in% c("Guernsey") ~ "Guernsey",
      country_raw %in% c("Jersey") ~ "Jersey",
      country_raw %in% c("Papua Niugini") ~ "Papua New Guinea",
      country_raw %in% c("اليمن") ~ "Yemen",
      country_raw %in% c("Cameroun") ~ "Cameroon",
      country_raw %in% c("Viti") ~ "Fiji",
      country_raw %in% c("العراق") ~ "Iraq",
      country_raw %in% c("Lëtzebuerg") ~ "Luxembourg",
      country_raw %in% c("မြန်မာ") ~ "Myanmar",
      country_raw %in% c("Guinée") ~ "Guinea",
      country_raw %in% c("Azərbaycan") ~ "Azerbaijan",

      # Swedish municipalities → Sweden
      stringr::str_detect(country_raw, "(?i)municipality") ~ "Sweden",

      # Unknown/invalid locations
      country_raw %in%
        c("AligudarS", "Coutchman", "The Bellmores") ~ NA_character_,

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
          "Northern Ireland",
          "Faroe Islands",
          "Guernsey",
          "Jersey",
          "Kosovo"
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
          "Panama",
          "Greenland",
          # Caribbean countries (geographically North America)
          "Cuba",
          "Jamaica",
          "Haiti",
          "Dominican Republic",
          "Bahamas",
          "Trinidad and Tobago",
          "Barbados",
          "Saint Kitts and Nevis",
          "Bermuda",
          "British Virgin Islands"
        ) ~ "North America",

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
          "Macau",
          "Georgia", # (transcontinental, but typically classified as Asia)
          "Armenia",
          "Azerbaijan"
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
          "Côte d’Ivoire",
          "Rwanda",
          "Burundi",
          "Democratic Republic of the Congo",
          "Benin",
          "Guinea",
          "Gambia",
          "Sierra Leone",
          "Gabon"
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

      # South America
      country %in%
        c(
          "Brazil",
          "Argentina",
          "Colombia",
          "Venezuela",
          "Chile",
          "Peru",
          "Ecuador",
          "Bolivia",
          "Uruguay",
          "Paraguay",
          "Guyana",
          "Suriname",
          "French Guiana"
        ) ~ "South America",

      TRUE ~ NA_character_
    )
  ) |>
  dplyr::select(original, country, continent)

# Save the lookup table
use_data(origin_lookup_final, internal = TRUE, overwrite = TRUE)
