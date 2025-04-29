# url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_2041_1.csv?date=latest&geography=645922819,645922820,645922822,645922823,645922817,645922818,645922821,645922825...645922828,645922824,645922833,645922834,645922829...645922832,645922835...645922840,645922851...645922862,645922841...645922850,645922863,645922864,645922866,645922865,645922867,645922869,645922868,645922870...645922890,645922892,645922891,645922922,645922923,645922893...645922921,645922924,645922927,645922926,645922925,645922928...645922940,645922948...645922953,645922941...645922947,645922957,645922956,645922958,645922959,645922954,645922955,645922960...645922978,645922980,645922981,645922984,645922985,645922987...645922996,645922982,645922986,645922979,645922983,645922997,645922998,645923000,645923013...645923017,645922999,645923018...645923021,645923001...645923003,645923022...645923025,645923004,645923005,645923026,645923006,645923007,645923027,645923008,645923028,645923029,645923009,645923030,645923010,645923031,645923011,645923012,645923035,645923032,645923041,645923038,645923039,645923042,645923043,645923036,645923033,645923037,645923040,645923034,645923044...645923097,645923100,645923102,645923101,645923104,645923103,645923098,645923099,645923105,645923114,645923115,645923106...645923113,645923116...645923131,645923133...645923141,645923143...645923147,645923132,645923142&c2021_eth_20=0,1001,12,13,10,11,14,1002,16,15,17,1003,8,7,6,9,1004,1...5,1005,18,19&measures=20100"

#
# date <- "date=latest"
#
# areas <- unique(df2$lad22cd)
#
# geography <- paste0("geography=",paste(areas, collapse = ","))
#
# # Ethnicity variable - this asks for all disaggregated and aggregated
# ethn <- "c2021_eth_20=0,1001,12,13,10,11,14,1002,16,15,17,1003,8,7,6,9,1004,1...5,1005,18,19"
#
# # Type of measure (value/percent)
# measures <- "measures=20100"
#
# base <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_2041_1.csv?"
#
# query <- paste0(base, date,"&",geography,"&",ethn,"&",measures)
#
# pop_ests <- readr::read_csv(query)
#
# response <- readr::read_csv(query)
# readr::read_csv(test)


#' Get population estimates
#'
#' Acquire population estimates broken down by ethnicity for the unique areas
#' in data via the NOMIS API. Uses an internal fetch function that communicates
#' with the NOMIS API.
#'
#' @param data A data frame or tibble of data. This will usually be be an output
#' of another policedatR function like get_data or analyse_data. The first column
#' must be the geography variable for which population estimates are desired.
#'
#' @returns A long-format tibble with population estimates for each ethnicity
#' for each (lowest-level) geography in data.
#'
#' @export
#'
#' @examples
#'
#' # Mock df
#' df <- data.frame("lad22cd" = c("E09000001","E09000002","E09000003"),
#'                  "x" = seq(1:3))
#'
#' population_estimates <- get_population_estimates(df)
#'
get_population_estimates <- function(data){

  # Set up caching
  policedatR::caching_check()
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  # If caching, memoise the fetch function, otherwise use as is
  if(caching){
    cd = cachem::cache_disk(cache_dir, evict = "lru")
    fetch_data <- memoise::memoise(fetch_population_estimates, cache = cd)
  }
  else{
    fetch_data <- fetch_population_estimates
  }

  t1 <- Sys.time()
  response <- fetch_data(data)
  raw_data <- httr::content(response, as = "text", encoding = "UTF-8")
  pop_ests <- readr::read_csv(raw_data,
                              show_col_types = FALSE) # silences readr message

  # Get area code colname to apply to output
  area_code <- colnames(data)[1]

  pop_ests <- pop_ests %>%
    janitor::clean_names() %>%
    dplyr::select(geography_code,
                  c2021_eth_20, # Keep the code as well as the name for easy filtering
                  c2021_eth_20_name,
                  obs_value)

  colnames(pop_ests) <- c(area_code, "ethnicity_code", "ethnicity", "population")

  # Simplify the ethnicity names
  pop_ests <- pop_ests %>%
    dplyr::mutate(
      ethnicity = dplyr::case_when(
        stringr::str_starts(ethnicity_code, "100") | ethnicity_code == 0 ~ # This is the aggregated categories
          stringr::word(ethnicity) %>%
            stringr::str_replace(.,",","") %>%
            stringr::str_replace(.,":","") %>%
            stringr::str_to_lower(),
        !stringr::str_starts(ethnicity_code, "100") & ethnicity_code != 0 ~ # This is disaggregated
          substr(ethnicity, stringr::str_locate(ethnicity, ":") + 2, nchar(ethnicity)) %>%
            stringr::str_replace_all(",","") %>%
            stringr::str_replace_all(" ","_") %>%
            stringr::str_to_lower(),
        TRUE ~ ethnicity)
    )


  # Merge 'gypsy or irish traveller' and 'roma' into 'other white' as per
  # "Census 2021 Ethnic group classification 6a". Police data does not have
  # the disaggregated categories
  pop_ests_wider <- pop_ests %>%
    tidyr::pivot_wider(id_cols = lad22cd, names_from = ethnicity, values_from = c(population)) %>%
    dplyr::mutate(
      other_white = other_white + gypsy_or_irish_traveller + roma
    ) %>%
    dplyr::select(-c(gypsy_or_irish_traveller, roma))

  pop_ests2 <- pop_ests_wider %>%
    tidyr::pivot_longer(cols = 2:ncol(.), names_to = "ethnicity", values_to = "population")

  pop_ests <- pop_ests %>%
    dplyr::select(-population) %>%
    dplyr::right_join(pop_ests2, by = c("lad22cd","ethnicity"))

  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units = "secs")
  cat(paste0("\nRequest done in: ", round(time_elapsed, 3), " seconds"))
  cat("\nNote time includes local data processing")

  return(pop_ests)

}
