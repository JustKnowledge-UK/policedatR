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
get_population_estimates <- function(data, collapse_ethnicity){

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


  # For PFAs we need to calculate population from constituent LADs.
  if(area_code == "pfa22cd"){

    colnames(pop_ests) <- c("lad22cd", "ethnicity_code", "ethnicity", "population")

    lookup <- area_lookup %>%
      dplyr::select(lad22cd, pfa22cd) %>%
      dplyr::distinct()

    pop_ests <- pop_ests %>%
      dplyr::left_join(lookup, by = "lad22cd") %>%
      dplyr::group_by(pfa22cd, ethnicity_code, ethnicity) %>%
      dplyr::summarise(
        population = sum(population)
      ) %>%
      dplyr::ungroup()


  }
  else{
    colnames(pop_ests) <- c(area_code, "ethnicity_code", "ethnicity", "population")
  }




  # Filter based on whether we want aggregated or disaggregated,
  # then simplify the ethnicity names
  if(collapse_ethnicity){
    pop_ests <- pop_ests %>%
      dplyr::filter(stringr::str_starts(ethnicity_code, "100")) %>%
      dplyr::mutate(
        ethnicity = stringr::word(ethnicity) %>%
            stringr::str_replace(.,",","") %>%
            stringr::str_replace(.,":","") %>%
            stringr::str_to_lower()
        ) %>%
      dplyr::select(-ethnicity_code)
  }
  else{
    pop_ests <- pop_ests %>%
      dplyr::filter(!stringr::str_starts(ethnicity_code, "100") & ethnicity_code != 0) %>%
      dplyr::mutate(
        ethnicity = substr(ethnicity, stringr::str_locate(ethnicity, ":") + 2, nchar(ethnicity)) %>%
              stringr::str_replace_all(",","") %>%
              stringr::str_replace_all(" ","_") %>%
              stringr::str_to_lower()
      ) %>%
      dplyr::select(-ethnicity_code)

    # Merge 'roma' into 'other white' because police data uses 18+1
    pop_ests <- pop_ests %>%
      tidyr::pivot_wider(id_cols = !!rlang::sym(area_code), names_from = ethnicity, values_from = c(population)) %>%
      dplyr::mutate(
        other_white = other_white + roma
      ) %>%
      dplyr::select(-roma) %>%
      tidyr::pivot_longer(cols = 2:ncol(.), names_to = "ethnicity", values_to = "population")


  }

  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units = "secs")
  cat(paste0("\nRequest done in: ", round(time_elapsed, 3), " seconds"))
  cat("\nNote time includes local data processing")

  return(pop_ests)

}
