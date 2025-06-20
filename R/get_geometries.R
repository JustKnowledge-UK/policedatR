#' Get Region geometries
#'
#' Acquire geometry data from geoportal.gov.uk for Regions as
#' at December 2022.
#'
#' @param subset A named list defining the areas for which to acquire geometries. Names
#' correspond to the area variable on which to subset (run policedatR::area_variables() to see options for areas on which to subset).
#' Values correspond to the desired values of the area variable. Values can be single
#' strings or character vectors.
#'
#' If the areas specified are at a lower level of geography, the Regions to which the lower geography belongs will be returned.
#'
#' If NULL, will acquire all Region geometries and will take around 1 minute pre-caching.
#'
#' @returns An sf data frame where each row is a Region, with columns rgn22cd, rgn22nm,
#' shape_area and geometry.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get all region geometries
#' # Don't run as takes ~12 seconds
#' region_geometries <- get_region_geometries()
#'
#'}
#' # Get the Region that Haringey is a part of
#' subset_geometries <- get_region_geometries(subset = list("lad22nm" = "Haringey"))
#'
#'
get_region_geometries <- function(subset = NULL,
                                  cache = TRUE){

  # Initialise and specify parameters

  # API endpoint
  api_endpoint <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Regions_December_2022_Boundaries_EN_BFC_V2/FeatureServer/0/query"
  # If subsetting, the size of the chunks to make
  batch_size <- 90
  # If subseting, the variable on which to subset
  target_variable <- "RGN22CD"

  # Create cache directory - but ask user to agree
  policedatR::caching_check(cache)
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  # If caching, memoise the fetch function, otherwise use as is
  if(caching){
    cd = cachem::cache_disk(cache_dir, evict = "lru")
    # Memoise the function
    fetch_data <- memoise::memoise(fetch_geometry_data, cache = cd)
  }
  else{
    fetch_data <- fetch_geometry_data
  }

  # Helper: Parse http response to sf and tidy
  parse_response <- function(response) {
    geojson_data <- httr::content(response, as = "text")
    # Translate to sf object
    sf::st_read(geojson_data, quiet = TRUE) %>%
      # Tidy up
      janitor::clean_names() %>%
      dplyr::select(dplyr::ends_with("cd"), dplyr::ends_with("nm"), shape_area, geometry)

  }

  # Main logic
  t1 <- Sys.time()
  cat("\nStarting request")

  # Subset options.
  if(is.null(subset)){
    # Run the request and parse the response
    response <- fetch_data(base_url = api_endpoint, where_clause = "1=1")
    geometries <- parse_response(response)

  }
  else{
    # If a subset has been requested, run in chunks of 90. This is limit set by API
    # First we need to get the relevant areas for the subset
    subset_variable <- names(subset)[1]
    subset_values <- subset[[1]]

    target_values <- area_lookup %>%
      dplyr::filter(!!rlang::sym(subset_variable) %in% subset_values) %>%
      dplyr::distinct(rgn22cd) %>%
      dplyr::pull()

    # Split the target values into chunks based on batch size
    chunks <- split(target_values, ceiling(seq_along(target_values) / batch_size))

    # For each chunk, fetch the data, parse it, and then bind all the rows together
    geometries <- purrr::map_dfr(chunks, function(chunk) {
      where_clause <- paste0(target_variable, " IN ('", paste(chunk, collapse = "', '"), "')")
      response <- fetch_data(base_url = api_endpoint, where_clause)
      parse_response(response)
    })
  }

  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units = "secs")
  cat(paste0("\nRequest done in: ", round(time_elapsed, 3), " seconds"))
  cat("\nNote time includes local data processing")

  return(geometries)

}

#' Get Police Force Area geometries
#'
#' Acquire geometry data from geoportal.gov.uk for Police Force Areas (PFAs) as
#' at December 2022.
#'
#' @param subset A named list defining the areas for which to acquire geometries. Names
#' correspond to the area variable on which to subset (run policedatR::area_variables() to see options for areas on which to subset).
#' Values correspond to the desired values of the area variable. Values can be single
#' strings or character vectors.
#'
#' If the areas specified are at a higher level of geography, all PFAs within the higher geographies will be returned.
#' If the areas specified are at a lower level of geography, the PFAs to which the lower geography belongs will be returned.
#'
#' If NULL, will acquire all PFA geometries and will take around 1 minute pre-caching.
#'
#' @returns An sf data frame where each row is a Local Authority District, with columns lad22cd, lad22nm,
#' shape_area and geometry.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get all PFA geometries
#' # Don't run as takes ~49 seconds
#' pfa_geometries <- get_pfa_geometries()
#'
#'}
#' # Get the PFA that Haringey is a part of
#' subset_geometries <- get_lad_geometries(subset = list("lad22nm" = "Haringey"))
#'
#'
get_pfa_geometries <- function(subset = NULL,
                               cache = TRUE){

  # API endpoint
  api_endpoint <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Police_Force_Areas_December_2022_EW_BFC/FeatureServer/0/query"
  # If subsetting, the size of the chunks to make
  batch_size <- 90
  # If subseting, the variable on which to subset
  target_variable <- "PFA22CD"

  # Create cache directory - but ask user to agree
  policedatR::caching_check(cache)
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  # If caching, memoise the fetch function, otherwise use as is
  if(caching){
    cd = cachem::cache_disk(cache_dir, evict = "lru")
    # Memoise the function
    fetch_data <- memoise::memoise(fetch_geometry_data, cache = cd)
  }
  else{
    fetch_data <- fetch_geometry_data
  }

  # Helper: Parse http response to sf and tidy
  parse_response <- function(response) {
    geojson_data <- httr::content(response, as = "text")
    # Translate to sf object
    sf::st_read(geojson_data, quiet = TRUE) %>%
      # Tidy up
      janitor::clean_names() %>%
      dplyr::select(dplyr::ends_with("cd"), dplyr::ends_with("nm"), shape_area, geometry)

  }

  t1 <- Sys.time()
  cat("\nStarting request")

  # Subset options. Build on this later
  if(is.null(subset)){
    # Run the request and parse the response
    response <- fetch_data(base_url = api_endpoint, where_clause = "1=1")
    geometries <- parse_response(response)

  }
  else{
    # If a subset has been requested, run in chunks of 90. This is limit set by API
    # First we need to get the relevant areas for the subset
    subset_variable <- names(subset)[1]
    # Use toupper because that is how it is on API
    subset_values <- subset[[1]]

    target_values <- area_lookup %>%
      dplyr::filter(!!rlang::sym(subset_variable) %in% subset_values) %>%
      dplyr::distinct(pfa22cd) %>%
      dplyr::pull()

    # Initialise and specify parameters for the query
    all_results <- list()


    chunks <- split(target_values, ceiling(seq_along(target_values) / batch_size))

    # For each chunk, fetch the data, parse it, and then bind all the rows together
    geometries <- purrr::map_dfr(chunks, function(chunk) {
      where_clause <- paste0(target_variable, " IN ('", paste(chunk, collapse = "', '"), "')")
      response <- fetch_data(base_url = api_endpoint, where_clause)
      parse_response(response)
    })
  }

  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units = "secs")
  cat(paste0("\nRequest done in: ", round(time_elapsed, 3), " seconds"))
  cat("\nNote time includes local data processing")

  return(geometries)

}

#' Get Local Authority District geometries
#'
#' Acquire geometry data from geoportal.gov.uk for Local Authority Districts as
#' at December 2022.
#'
#' @param subset A named list defining the areas for which to acquire geometries. Names
#' correspond to the area variable on which to subset (run policedatR::area_variables() to see options for areas on which to subset).
#' Values correspond to the desired values of the area variable. Values can be single
#' strings or character vectors.
#'
#' If NULL, will acquire all LAD geometries and will take around 1 minute pre-caching.
#'
#' @returns An sf data frame where each row is a Local Authority District, with columns lad22cd, lad22nm,
#' shape_area and geometry.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get all LAD geometries
#' # Don't run as will take a while.
#' lad_geometries <- get_lad_geometries()
#'}
#' # Get just Haringey and Lambeth geometries using 'lad22nm' variable
#' subset_geometries <- get_lad_geometries(subset = list("lad22nm" = c("Haringey", "Lambeth")))
#'
#' # Get just Haringey and Lambeth geometries using 'lad22cd' variable
#' subset_geometries <- get_lad_geometries(subset = list("lad22cd" = c("E09000014", "E09000022")))
#'
get_lad_geometries <- function(subset = NULL,
                               cache = TRUE){

  # API endpoint
  api_endpoint <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Local_Authority_Districts_December_2022_UK_BFC_V2/FeatureServer/0/query"
  result_offset <- 0 # We may want the user to be able specify this
  max_records <- 400 # We may want the user to be able specify this
  # For subsetting, the size of the batch
  batch_size <- 90
  # For subsetting, the variable on which to subset
  target_variable <- "LAD22CD"

  # Create cache directory - but ask user to agree
  policedatR::caching_check(cache)
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  # If caching, memoise the fetch function, otherwise use as is
  if(caching){
    cd = cachem::cache_disk(cache_dir, evict = "lru")
    # Memoise the function
    fetch_data <- memoise::memoise(fetch_geometry_data, cache = cd)
  }
  else{
    fetch_data <- fetch_geometry_data
  }

  # Helper to process and clean each chunk
  process_chunk <- function(response) {
    geojson_data <- httr::content(response, as = "text")
    sf::st_read(geojson_data, quiet = TRUE)
  }

  # Helper to clean and filter the final geometry
  tidy_lad_geometries <- function(data) {
    data %>%
      janitor::clean_names() %>%
      dplyr::filter(!stringr::str_starts(lad22cd, "S"),
                    !stringr::str_starts(lad22cd, "N")) %>%
      dplyr::select(lad22cd, lad22nm, shape_area, geometry)
  }


  t1 <- Sys.time()
  cat("\nStarting request")
  # Subset options.
  if(is.null(subset)){
    # If no subset has been requested, acquire all data
    all_results <- list()
    # Repeat the request in 400 record batches
    # Sometimes the API doesn't like doing lots of records in one and will throw a
    # 504 (timeout). So we keep retrying, each time halving the requested payload.
    rep <- 1
    max_tries <- 10

    repeat{
      cat(paste0("\nStarting page ", rep))
      attempt <- 1
      local_max <- max_records

      # While loop repeats request with payload halved each time until it succeeds
      # or reaches max_tries
      while(attempt <= max_tries){
        cat(paste0("\nAttempt ", attempt))
        cat(paste0("\nTrying with max records = ", local_max))
        response <- try(
          fetch_data(base_url = api_endpoint,
                     where_clause = "1=1",
                     result_offset,
                     local_max)
        )

        if(!inherits(response, "try-error")){
          cat(paste0("\nSuccess with max records = ", local_max))
          break
        }

        attempt <- attempt + 1
        local_max <- floor(local_max / 2) # try again by halving the payload
        cat("\nRetrying...")
      }

      # Translate to sf object
      sf_chunk <- process_chunk(response)
      # Stop if no more records
      if (nrow(sf_chunk) == 0) break
      # Append results
      all_results <- append(all_results, list(sf_chunk))
      result_offset <- result_offset + local_max
      rep <- rep + 1
    }

    # Bind all rows and tidy
    geometries <- dplyr::bind_rows(all_results) %>%
      tidy_lad_geometries()
  }
  else{
    # If a subset has been requested, run in chunks of 90. This is limit set by API
    # First we need to get the relevant areas for the subset
    subset_variable <- names(subset)[1]
    subset_values <- subset[[1]]

    target_values <- area_lookup %>%
      dplyr::filter(!!rlang::sym(subset_variable) %in% subset_values) %>%
      dplyr::distinct(lad22cd) %>%
      dplyr::pull()

    all_results <- list()

    chunks <- split(target_values, ceiling(seq_along(target_values) / batch_size))

    # Helper: Run query with where clause and process
    run_query <- function(chunk) {
      where_clause <- paste0(target_variable, " IN ('", paste(chunk, collapse = "', '"), "')")
      response <- fetch_data(api_endpoint, where_clause)
      process_chunk(response)
    }

    # For each chunk, run the query, process it, and row bind, then tidy
    geometries <- purrr::map_dfr(chunks, run_query) %>% tidy_lad_geometries()

  }

  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units = "secs")
  cat(paste0("\nRequest done in: ", round(time_elapsed, 3), " seconds"))
  cat("\nNote time includes local data processing")

  return(geometries)

}



#' Get Middle layer Super Output Area geometries
#'
#' Acquire geometry data from geoportal.gov.uk for Middle layer Super Output Area (MSOAs) as
#' at December 2021.
#'
#' @param subset A named list defining the areas for which to acquire geometries. Names
#' correspond to the area variable on which to subset (run policedatR::area_variables() to see options for areas on which to subset).
#' Values correspond to the desired values of the area variable. Values can be single
#' strings or character vectors.
#'
#' If NULL, will acquire all MSOA geometries and will take around 5 minutes pre-caching.
#'
#' @returns An sf data frame where each row is a MSOA, with columns msoa21cd, msoa21nm,
#' shape_area and geometry.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get all MSOAs
#' # Don't run as will take a long time
#' all_msoas <- get_msoa_geometries()
#'
#' # Get MSOAs in the Greater London region
#' # Don't run as will take ~31 seconds
#' london_msoas <- get_msoa_geometries(subset = list("rgn22nm" = "London"))
#'
#'}
#'
#' # Get MSOAs just in Haringey and Waltham Forest
#' hw_msoas <- get_msoa_geometries(
#'                 subset = list("lad22nm" = c("Haringey", "Waltham Forest")))
#'
#'
get_msoa_geometries <- function(subset = NULL,
                                cache = TRUE){

  api_endpoint <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Middle_layer_Super_Output_Areas_December_2021_Boundaries_EW_BFC_V7/FeatureServer/0/query"
  result_offset <- 0 # We may want the user to be able specify this
  max_records <- 2000 # We may want the user to be able specify this
  # For subsetting, the size of the batch
  batch_size <- 90
  # For subsetting, the variable on which to subset
  target_variable <- "MSOA21CD"

  # Create cache directory - but ask user to agree
  policedatR::caching_check(cache)
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  # If caching, memoise the fetch function, otherwise use as is
  if(caching){
    cd = cachem::cache_disk(cache_dir, evict = "lru")
    # Memoise the function
    fetch_data <- memoise::memoise(fetch_geometry_data, cache = cd)
  }
  else{
    fetch_data <- fetch_geometry_data
  }

  # Helper to process and clean each chunk
  process_chunk <- function(response) {
    geojson_data <- httr::content(response, as = "text")
    sf::st_read(geojson_data, quiet = TRUE)
  }

  # Helper to clean and filter the final geometry
  tidy_geometries <- function(data) {
    data %>%
      janitor::clean_names() %>%
      dplyr::select(dplyr::ends_with("cd"), dplyr::ends_with("nm"), shape_area, geometry)
  }

  t1 <- Sys.time()
  cat("\nStarting request.")

  # result_offset <- 0
  # max_records <- 2000
  # Subset options.
  if(is.null(subset)){
    all_results <- list()
    # If no subset has been requested, acquire all data
    # Here pagination is required as max records in single response is 2000


    # Repeat the request in 2000 record batches
    # Sometimes the API doesn't like doing 2000 records in one and will throw a
    # 504 (timeout). So we keep retrying, each time halving the requested payload.
    rep <- 1
    max_tries <- 10

    # Repeat across 2000 records a time, reducing the payload where timeouts occur.
    repeat{
      cat(paste0("\nStarting page ", rep))
      attempt <- 1
      local_max <- max_records

      # While loop repeats request with payload halved each time until it succeeds
      # or reaches max_tries
      while(attempt <= max_tries){
        cat(paste0("\nAttempt ", attempt))
        cat(paste0("\nTrying with max records = ", local_max))
        response <- try(
          fetch_data(base_url = api_endpoint,
                     where_clause = "1=1",
                     result_offset,
                     local_max)
        )

        if(!inherits(response, "try-error")){
          cat(paste0("\nSuccess with max records = ", local_max))
          break
        }

        attempt <- attempt + 1
        local_max <- floor(local_max / 2) # try again by halving the payload
        cat("\nRetrying...")
      }

      # Translate to sf object
      sf_chunk <- process_chunk(response)
      # Stop if no more records
      if (nrow(sf_chunk) == 0) break
      # Append results
      all_results <- append(all_results, list(sf_chunk))
      result_offset <- result_offset + local_max
      rep <- rep + 1
    }

    # Bind rows and tidy
    geometries <- dplyr::bind_rows(all_results) %>%
      tidy_geometries()
  }
  else{
    # If a subset has been requested, run in chunks of 90. This is limit set by API
    # First we need to get the relevant areas for the subset
    subset_variable <- names(subset)[1]
    # Use toupper because that is how it is on API
    subset_values <- subset[[1]]

    target_values <- area_lookup %>%
      dplyr::filter(!!rlang::sym(subset_variable) %in% subset_values) %>%
      dplyr::distinct(msoa21cd) %>%
      dplyr::pull()

    # Initialise and specify parameters for the query
    all_results <- list()

    chunks <- split(target_values, ceiling(seq_along(target_values) / batch_size))

    # Helper: Run query with where clause and process
    run_query <- function(chunk) {
      where_clause <- paste0(target_variable, " IN ('", paste(chunk, collapse = "', '"), "')")
      response <- fetch_data(api_endpoint, where_clause)
      process_chunk(response)
    }

    # For each chunk, run the query, process it, and row bind, then tidy
    geometries <- purrr::map_dfr(chunks, run_query) %>% tidy_geometries()


  }

  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units="secs")
  cat(paste0("\nRequest done in: ", round(time_elapsed,3), " seconds"))


  return(geometries)

}


#' Get Lower layer Super Output Area geometries
#'
#' Acquire geometry data from geoportal.gov.uk for Lower layer Super Output Area (LSOAs) as
#' at December 2021.
#'
#' @param subset A named list defining the areas for which to acquire geometries. Names
#' correspond to the area variable on which to subset (run policedatR::area_variables() to see options for areas on which to subset).
#' Values correspond to the desired values of the area variable. Values can be single
#' strings or character vectors.
#'
#'
#' If NULL, will acquire all LSOA geometries and will take around 5 minutes pre-caching.
#'
#' @returns An sf data frame where each row is a LSOA, with columns lsoa21cd, lsoa21nm,
#' shape_area and geometry.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get all LSOAs
#' # Don't run as will take a long time
#' all_lsoas <- get_lsoa_geometries()
#'
#' # Get LSOAs in the Greater London region
#' # Don't run as will take a while
#' london_lsoas <- get_lsoa_geometries(subset = list("rgn22nm" = "London"))
#'}
#' # Get LSOAs just in Haringey and Waltham Forest
#' hw_lsoas <- get_lsoa_geometries(
#'                 subset = list("lad22nm" = c("Haringey", "Waltham Forest"))
#'                 )
#'
#'
get_lsoa_geometries <- function(subset = NULL,
                                cache = TRUE){

  api_endpoint <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BFC_V10/FeatureServer/0/query"
  result_offset <- 0 # We may want the user to be able specify this
  max_records <- 2000 # We may want the user to be able specify this
  # For subsetting, the size of the batch
  batch_size <- 90
  # For subsetting, the variable on which to subset
  target_variable <- "LSOA21CD"

  # Create cache directory - but ask user to agree
  policedatR::caching_check(cache)
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  # If caching, memoise the fetch function, otherwise use as is
  if(caching){
    cd = cachem::cache_disk(cache_dir, evict = "lru")
    # Memoise the function
    fetch_data <- memoise::memoise(fetch_geometry_data, cache = cd)
  }
  else{
    fetch_data <- fetch_geometry_data
  }

  # Helper to process and clean each chunk
  process_chunk <- function(response) {
    geojson_data <- httr::content(response, as = "text")
    sf::st_read(geojson_data, quiet = TRUE)
  }

  # Helper to clean and filter the final geometry
  tidy_geometries <- function(data) {
    data %>%
      janitor::clean_names() %>%
      dplyr::select(dplyr::ends_with("cd"), dplyr::ends_with("nm"), shape_area, geometry)
  }


  t1 <- Sys.time()
  cat("\nStarting request.")
  # result_offset <- 0
  # max_records <- 2000

  # Subset options. Build on this later
  if(is.null(subset)){
    all_results <- list()
    # If no subset has been requested, acquire all data
    # Here pagination is required as max records in single response is 2000

    # Repeat the request in 2000 record batches
    # Sometimes the API doesn't like doing 2000 records in one and will throw a
    # 504 (timeout). So we keep retrying, each time halving the requested payload.
    rep <- 1
    max_tries <- 10

    # Repeat across 2000 records a time, reducing the payload where timeouts occur.
    repeat{
      cat(paste0("\nStarting page ", rep))
      attempt <- 1
      local_max <- max_records

      # While loop repeats request with payload halved each time until it succeeds
      # or reaches max_tries
      while(attempt <= max_tries){
        cat(paste0("\nAttempt ", attempt))
        cat(paste0("\nTrying with max records = ", local_max))
        response <- try(
          fetch_data(base_url = api_endpoint,
                     where_clause = "1=1",
                     result_offset,
                     local_max)
        )

        if(!inherits(response, "try-error")){
          cat(paste0("\nSuccess with max records = ", local_max))
          break
        }

        attempt <- attempt + 1
        local_max <- floor(local_max / 2) # try again by halving the payload
        cat("\nRetrying...")
      }

      # Translate to sf object
      sf_chunk <- process_chunk(response)
      # Stop if no more records
      if (nrow(sf_chunk) == 0) break
      # Append results
      all_results <- append(all_results, list(sf_chunk))
      result_offset <- result_offset + local_max
      rep <- rep + 1
    }

    # Bind rows and tidy
    geometries <- dplyr::bind_rows(all_results) %>%
      tidy_geometries()
  }
  else{
    # If a subset has been requested, run in chunks of 90. This is limit set by API

    # First we need to get the relevant areas for the subset
    subset_variable <- names(subset)[1]
    # Use toupper because that is how it is on API
    subset_values <- subset[[1]]

    target_values <- area_lookup %>%
      dplyr::filter(!!rlang::sym(subset_variable) %in% subset_values) %>%
      dplyr::distinct(lsoa21cd) %>%
      dplyr::pull()

    # Initialise and specify parameters for the query
    all_results <- list()

    chunks <- split(target_values, ceiling(seq_along(target_values) / batch_size))

    # Helper: Run query with where clause and process
    run_query <- function(chunk) {
      where_clause <- paste0(target_variable, " IN ('", paste(chunk, collapse = "', '"), "')")
      response <- fetch_data(api_endpoint, where_clause)
      process_chunk(response)
    }

    # For each chunk, run the query, process it, and row bind, then tidy
    geometries <- purrr::map_dfr(chunks, run_query) %>% tidy_geometries()


  }

  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units="secs")
  cat(paste0("\nRequest done in: ", round(time_elapsed,3), " seconds"))

  return(geometries)

}


#' Get Output Area geometries
#'
#' Acquire geometry data from geoportal.gov.uk for Output Area (OAs) as
#' at December 2021.
#'
#' @param subset A named list defining the areas for which to acquire geometries. Names
#' correspond to the area variable on which to subset (run policedatR::area_variables() to see options for areas on which to subset).
#' Values correspond to the desired values of the area variable. Values can be single
#' strings or character vectors.
#'
#'
#' If NULL, will acquire all OA geometries and will take around 5 minutes.
#'
#' @returns An sf data frame where each row is an  OA, with columns oa21cd, shape_area and geometry.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get all OAs
#' # Don't run as will take a long time
#' all_oas <- get_oa_geometries()
#'
#' # Get OAs in the Greater London region
#' # Don't run as will take a long time
#' london_oas <- get_oa_geometries(subset = list("rgn22nm" = "London"))
#'}
#'
#' # Get OAs just in Haringey and Waltham Forest
#' haringey_oas <- get_oa_geometries(subset = list("lad22nm" = "Haringey"))
#'
#'
get_oa_geometries <- function(subset = NULL,
                              cache = TRUE){

  api_endpoint <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Output_Areas_2021_EW_BFC_V8/FeatureServer/0/query"
  result_offset <- 0 # We may want the user to be able specify this
  max_records <- 2000 # We may want the user to be able specify this
  # For subsetting, the size of the batch
  batch_size <- 90
  # For subsetting, the variable on which to subset
  target_variable <- "OA21CD"

  # Create cache directory - but ask user to agree
  policedatR::caching_check(cache)
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  # If caching, memoise the fetch function, otherwise use as is
  if(caching){
    cd = cachem::cache_disk(cache_dir, evict = "lru")
    # Memoise the function
    fetch_data <- memoise::memoise(fetch_geometry_data, cache = cd)
  }
  else{
    fetch_data <- fetch_geometry_data
  }

  # Helper to process and clean each chunk
  process_chunk <- function(response) {
    geojson_data <- httr::content(response, as = "text")
    sf::st_read(geojson_data, quiet = TRUE)
  }

  # Helper to clean and filter the final geometry
  tidy_geometries <- function(data) {
    data %>%
      janitor::clean_names() %>%
      dplyr::select(oa21cd, shape_area, geometry)
  }

  t1 <- Sys.time()
  cat("\nStarting request.")


  # Subset options. Build on this later
  if(is.null(subset)){
    all_results <- list()
    # If no subset has been requested, acquire all data
    # Here pagination is required as max records in single response is 2000

    # Repeat the request in 2000 record batches
    # Sometimes the API doesn't like doing 2000 records in one and will throw a
    # 504 (timeout). So we keep retrying, each time halving the requested payload.
    rep <- 1
    max_tries <- 10

    # Repeat across 2000 records a time, reducing the payload where timeouts occur.
    repeat{
      cat(paste0("\nStarting page ", rep))
      attempt <- 1
      local_max <- max_records

      # While loop repeats request with payload halved each time until it succeeds
      # or reaches max_tries
      while(attempt <= max_tries){
        cat(paste0("\nAttempt ", attempt))
        cat(paste0("\nTrying with max records = ", local_max))
        response <- try(
          fetch_data(base_url = api_endpoint,
                     where_clause="1=1",
                     result_offset,
                     local_max)
        )

        if(!inherits(response, "try-error")){
          cat(paste0("\nSuccess with max records = ", local_max))
          break
        }

        attempt <- attempt + 1
        local_max <- floor(local_max / 2) # try again by halving the payload
        cat("\nRetrying...")
      }

      # Translate to sf object
      sf_chunk <- process_chunk(response)
      # Stop if no more records
      if (nrow(sf_chunk) == 0) break
      # Append results
      all_results <- append(all_results, list(sf_chunk))
      result_offset <- result_offset + local_max
      rep <- rep + 1

    }

    # Bind rows and tidy
    geometries <- dplyr::bind_rows(all_results) %>%
      tidy_geometries()

  }
  else{
    # If a subset has been requested, run in chunks of 90. This is limit set by API

    # First we need to get the relevant areas for the subset
    subset_variable <- names(subset)[1]
    # Use toupper because that is how it is on API
    subset_values <- subset[[1]]

    target_values <- area_lookup %>%
      dplyr::filter(!!rlang::sym(subset_variable) %in% subset_values) %>%
      dplyr::distinct(oa21cd) %>%
      dplyr::pull()

    all_results <- list()

    chunks <- split(target_values, ceiling(seq_along(target_values) / batch_size))

    # Helper: Run query with where clause and process
    run_query <- function(chunk) {
      where_clause <- paste0(target_variable, " IN ('", paste(chunk, collapse = "', '"), "')")
      response <- fetch_data(api_endpoint, where_clause)
      process_chunk(response)
    }

    # For each chunk, run the query, process it, and row bind, then tidy
    geometries <- purrr::map_dfr(chunks, run_query) %>% tidy_geometries()

  }

  t2 <- Sys.time()
  time_elapsed <- difftime(t2,t1, units = "secs")
  cat(paste0("\nRequest done in: ", round(time_elapsed, 3), " seconds"))

  return(geometries)

}

