# 2025-03-11
# Jolyon Miles-Wilson
# A script for compiling the geography lookup

rm(list = ls())
library(httr)
library(jsonlite)
library(dplyr)
library(janitor)
library(stringr)

###################################
#### OA to LSOA to MSOA to LAD ####
###################################

# These are 2021 OAs and 2022 LADs (post 2021 changes, pre 2023 changes)

# API endpoint
base_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/OA_LSOA_MSOA_EW_DEC_2021_LU_v3/FeatureServer/0/query"

# Initialize variables
result_offset <- 0
max_records <- 1000 # GeoPortal limits max records at a time to 2000
all_results <- list()

repeat {
  # Define query parameters with pagination
  params <- list(
    where = "1=1",
    outFields = "*",
    outSR = "4326",
    f = "json",
    resultOffset = result_offset,
    resultRecordCount = max_records
  )

  # Send request
  response <- httr::GET(base_url, query = params)

  if (httr::status_code(response) == 200) {
    # Get content
    json_data <- httr::content(response, as = "text")
    # Read from JSON
    chunk <- jsonlite::fromJSON(json_data, flatten = TRUE)
    # Convert to tibble
    tibble_chunk <- tibble::as_tibble(chunk$features)

    # Stop if no more records
    if (nrow(tibble_chunk) == 0) break

    # Append results
    all_results <- base::append(all_results, list(tibble_chunk))

    # Advance the offset for next batch
    result_offset <- result_offset + max_records
  } else {
    stop(paste("Error:", httr::status_code(response)))
  }
}

# Combine all results into a single df
final_df <- dplyr::bind_rows(all_results)

# Tidy up the df
oa_lsoa_msoa_lad <- final_df %>%
  janitor::clean_names() %>%
  dplyr::rename_with(~stringr::str_replace(.x, "attributes.","")) %>%
  dplyr::select(-c(lsoa21nmw, msoa21nmw,lad22nmw, object_id))


###########################
#### LAD to PFA lookup ####
###########################

# Ideally we'd keep CSPs as they could be useful in future. But there is a many-
# to-many relationship between LADs and CSPs, so for convenience I have decided
# to drop them.

# Becuase there are only 331 LADs there is no need to paginate

base_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/LAD22_CSP22_PFA22_EW_LU/FeatureServer/0/query"

params <- list(
  where = "1=1",
  outFields = "*",
  f = "json"
)

response <- httr::GET(base_url, query = params)

# Get JSON data
json_data <- httr::content(response, as = "text")
# Read JSON data
list_data <- jsonlite::fromJSON(json_data, flatten = TRUE)

# Convert to tibble and tidy
lad_pfa <- tibble::as_tibble(list_data$features) %>%
  janitor::clean_names() %>%
  dplyr::rename_with(~stringr::str_replace(.x, "attributes.","")) %>%
  select(-c(csp22cd, csp22nm, object_id)) %>%
  distinct()


#######################
#### LAD to Region ####
#######################

# No need to paginate as LADS = 309

base_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/LAD22_RGN22_EN_LU/FeatureServer/0/query"

params <- list(
  where = "1=1",
  outFields = "*",
  f = "json"
)

response <- httr::GET(base_url, query = params)

# Get JSON data
json_data <- httr::content(response, as = "text")
# Read JSON data
list_data <- jsonlite::fromJSON(json_data, flatten = TRUE)
# Convert to tibble and tidy
lad_region <- tibble::as_tibble(list_data$features) %>%
  janitor::clean_names() %>%
  dplyr::rename_with(~stringr::str_replace(.x, "attributes.","")) %>%
  select(-object_id)


#############################
#### Combine the lookups ####
#############################

area_lookup <- oa_lsoa_msoa_lad %>%
  left_join(lad_pfa %>% dplyr::select(-lad22nm), by = "lad22cd") %>% # drop lad22nm as it differs slightly in pfa lookup (e.g. 'city of')
  left_join(lad_region %>% dplyr::select(-lad22nm), by = "lad22cd") %>% # drop lad22nm as it differs slightly in rgn lookup (e.g. 'city of')
  # "Fill in" the region for Welsh areas as 'Wales'
  mutate(
    rgn22nm = ifelse(stringr::str_starts(lad22cd, "W"), "Wales", rgn22nm),
    rgn22cd = ifelse(stringr::str_starts(lad22cd, "W"), "Wales", rgn22cd),
  )

# Add the data to sysdata.rda to be used by the package (but not accessible by
# user; cf exported data)
usethis::use_data(area_lookup,
                  # add other data here
                  internal = TRUE,
                  overwrite = TRUE)

