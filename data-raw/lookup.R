# 2025-03-11
# A script for compiling the geography lookup

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(stringr)
library(sf)

# API endpoint
base_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/OA_LSOA_MSOA_EW_DEC_2021_LU_v3/FeatureServer/0/query"

# Initialize variables
result_offset <- 0
max_records <- 1000 # GeoPortal limits max records at a time to 2000
all_results <- list()
# all_results2 <- c()
# batch_size <- 90 # GeoPortal limits max WHERE to 90

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
    json_data <- httr::content(response, as = "text")
    chunk <- jsonlite::fromJSON(json_data, flatten = TRUE)
    tibble_chunk <- tibble::as_tibble(chunk$features)

    # Stop if no more records
    if (nrow(tibble_chunk) == 0) break

    all_results <- base::append(all_results, list(tibble_chunk))
    result_offset <- result_offset + max_records
  } else {
    stop(paste("Error:", httr::status_code(response)))
  }
}

# Combine all results into a single sf object
final_df <- dplyr::bind_rows(all_results)

final_df <- final_df %>%
  janitor::clean_names() %>%
  dplyr::rename_with(~stringr::str_replace(.x, "attributes.","")) %>%
  dplyr::select(-c(lsoa21nmw, msoa21nmw,lad22nmw, object_id))


### add below programatically!
###########################
#### LAD to PFA lookup ####
###########################

# Use list.files to get all CSV files in the directory
files <- list.files(path = path, pattern = 'Local_Authority_District_to_CSPs*', full.names = TRUE)

lad_pfa_lookup <- readr::read_csv(files[1]) %>%
  janitor::clean_names() %>%
  select(lad23cd, lad23nm, pfa23cd, pfa23nm) %>%
  distinct()

#######################
#### LAD to Region ####
#######################

# Use list.files to get all CSV files in the directory
files <- list.files(path = path, pattern = 'Local_Authority_District_to_Region*', full.names = TRUE)

lad_region_lookup <- readr::read_csv(files[1]) %>%
  janitor::clean_names() %>%
  select(-object_id) %>%
  distinct()

#############################
#### Combine the lookups ####
#############################

# combine lad with region first to inspect difference in number of lads
# shows that 'region' is missing from wales, so we set 'Wales' as the region
lad_pfa_region <- lad_pfa_lookup %>%
  left_join(lad_region_lookup %>% select(-lad23nm), by="lad23cd") %>%
  mutate(
    rgn23nm = ifelse(stringr::str_starts(lad23cd, "W"), "Wales", rgn23nm)
  )

# now combine with the lsoas
combined_geos <- lsoa_lad_lookup %>%
  left_join(lad_pfa_region %>% select(-'lad23nm'), by='lad23cd')

