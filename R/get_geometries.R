#' Get Local Authority District geometries
#'
#' Acquire geometry data from geoportal.gov.uk for Local Authority Districts as
#' at December 2022.
#'
#' @param subset A named list defining the areas to which to subset. Names
#' correspond to the variable on which to subset. Values correspond to the desired
#' values of the variable.
#'
#' @returns A tibble of geometries where each row is local authority district
#' @export
#'
#' @examples
#'
#' lad_geometries <- get_lad_geometries()
#'
#' # Get just Haringey and Lambeth geometries using 'lad22nm' variable
#' subset_geometries <- get_lad_geometries(subset = list("lad22nm" = c("Haringey", "Lambeth")))
#'
#' # Get just Haringey and Lambeth geometries using 'lad22cd' variable
#' subset_geometries <- get_lad_geometries(subset = list("lad22cd" = c("E09000014", "E09000022")))
#'
get_lad_geometries <- function(subset = NULL){

  # API endpoint
  base_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Local_Authority_Districts_December_2022_UK_BFC_V2/FeatureServer/0/query"

  # Create cache directory - but ask user to agree
  policedatR::caching_check()
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  # Specify the request as a function so it can be memoised
  # Note we need to do something with the status code so that non-200 responses
  # don't get accepted and memoised. I've got a solution but not yet implemented
  fetch_data <- function(where_clause) {
    params <- list(
      where = where_clause,  # Retrieve all records
      outFields =  "*", # "*" specifies all. I tried specifying fewer outfields but it doesn't speed things up. No need to specify geometry as this is assumed by default
      outSR = "4326",
      f = "geojson"
    )

    response <- httr::GET(base_url, query = params)
    if (httr::status_code(response) == 200) {
      #content(response, "text")  # Convert response to text for caching
      return(response)
    }
    else {
      stop(paste("Error:", httr::status_code(response)))
    }
  }


  if(caching){
    cd = cachem::cache_disk(cache_dir, evict = "lru")
    # Memoise the function
    fetch_data <- memoise::memoise(fetch_data, cache = cd)
  }

    # Subset options. Build on this later
  if(is.null(subset)){
    # If no subset has been requested, acquire all data
    where_clause <- "1=1" # we could build options here

    t1 <- Sys.time()
    cat("\nStarting request")
    response <- fetch_data(where_clause)
    t2 <- Sys.time()
    time_elapsed <- t2 - t1
    cat(paste0("\nRequest done in: ", time_elapsed, " seconds"))

    geojson_data <- httr::content(response, as = "text")
    # Translate to sf object
    lad_geometries <- sf::st_read(geojson_data, quiet = TRUE) %>%
      # Tidy up
      janitor::clean_names() %>%
      dplyr::select(lad22cd, lad22nm, shape_area, geometry)
  }
  else{
    # If a subset has been requested, run in chunks of 90. This is limit set by API

    # Initialise and specify parameters for the query
    all_results <- list()
    batch_size <- 90

    target_variable <- toupper(names(subset)[1]) # Use toupper because that is how it is on API
    target_values <- subset[[1]]

    chunks <- split(target_values, ceiling(seq_along(target_values) / batch_size))

    t1 <- Sys.time()
    cat("\nStarting request")
    for(chunk in chunks){
      # Update the where clause with the next batch
      where_clause <- paste0(target_variable, " IN ('", paste(chunk, collapse = "', '"), "')")

      # Run the query
      response <- fetch_data(where_clause)  # First call fetches from API

      # Get the contents
      geojson_data <- httr::content(response, as = "text")

      # Translate to sf object
      sf_chunk <- sf::st_read(geojson_data, quiet = TRUE)

      # Append results
      all_results <- append(all_results, list(sf_chunk))


    }
    t2 <- Sys.time()
    time_elapsed <- t2 - t1
    cat(paste0("\nRequest done in: ", time_elapsed, " seconds"))

    lad_geometries <- dplyr::bind_rows(all_results) %>%
      # Tidy up
      janitor::clean_names() %>%
        dplyr::select(lad22cd, lad22nm, shape_area, geometry)
  }


  return(lad_geometries)

}



#' Get Middle layer Super Output Area geometries
#'
#' Acquire geometry data from geoportal.gov.uk for Middle layer Super Output Area (MSOAs) as
#' at December 2021.
#'
#' @param subset A named list defining the areas for which to acquire geometries. Names
#' correspond to the variable on which to subset (run policedatR::area_variables() to see options for areas on which to subset).
#' Values correspond to the desired values of the variable. Values can be single
#' strings or character vectors.
#'
#' If NULL, will acquire all MSOA geometries and will take around 5 minutes.
#'
#' @returns An sf data frame where each row is a MSOA, with columns msoa21cd, msoa21nm,
#' shape_area and geometry.
#'
#' @export
#'
#' @examples
#'
#' # Get all MSOAs
#' all_msoas <- get_msoa_geometries()
#'
#' # Get MSOAs in the Greater London region
#' london_msoas <- get_msoa_geometries(subset = list("rgn22nm" = "London"))
#'
#' # Get MSOAs just in Haringey and Waltham Forest
#' haringey_waltham_msoas <- get_msoa_geometries(subset = list("lad22nm" = c("Haringey", "Waltham Forest")))
#'
get_msoa_geometries <- function(subset = NULL){

  api_endpoint <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Middle_layer_Super_Output_Areas_December_2021_Boundaries_EW_BFC_V7/FeatureServer/0/query"
  result_offset <- 0 # We may want the user to be able specify this
  max_records <- 2000 # We may want the user to be able specify this
  # Create cache directory - but ask user to agree
  policedatR::caching_check()
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  # Specify the request as a function so it can be memoised
  # Note we need to do something with the status code so that non-200 responses
  # don't get accepted and memoised. I've got a solution but not yet implemented
  fetch_data <- function(base_url,
                         where_clause,
                         result_offset,
                         max_records # GeoPortal max records in single response is 2000
                         ) {
    params <- list(
      where = where_clause,  # Retrieve all records
      outFields =  "*", # "*" specifies all. I tried specifying fewer outfields but it doesn't speed things up. No need to specify geometry as this is assumed by default
      outSR = "4326",
      f = "geojson",
      resultOffset = result_offset, # for pagination
      resultRecordCount = max_records
    )

    # Initialise 504. Geoportal API saves 504 error to response content, so we
    # have to look there for the error. But let's only look when it seems likely
    # the request failed. we do this by using the length of the response - if it
    # looks really short, it's probably failed, so take a look and see if indeed
    # an error is reported
    error_504 <- FALSE

    response <- httr::GET(base_url, query = params)

    if(length(response[["content"]]) < 1000){
      peek_content <- httr::content(response, as = "text")
      if(grepl("error", peek_content, ignore.case = TRUE) && grepl("504", peek_content)){
        error_504 <- TRUE
        #max_records <- max_records / 2
      }
      else {
        error_504 <- FALSE
      }
    }
    if (httr::status_code(response) == 200 && error_504 == FALSE) { # add check in body for 504
      return(response)
      #content(response, "text")  # Convert response to text for caching
    }
    else {
      stop(paste("Error: Status code ", httr::status_code(response)," (but body may contain 504)"))
    }
  }

  if(caching){
    cd = cachem::cache_disk(cache_dir, evict = "lru")

    # Memoise the function
    fetch_data <- memoise::memoise(fetch_data, cache = cd)
  }

  # Initialise
  all_results <- list()
  # result_offset <- 0
  # max_records <- 2000

  # Subset options. Build on this later
  if(is.null(subset)){
    # If no subset has been requested, acquire all data
    # Here pagination is required as max records in single response is 2000
    where_clause <- "1=1" # we could build options here

    t1 <- Sys.time()
    cat("\nStarting request.")

    # Repeat the request in 2000 record batches
    # Sometimes the API doesn't like doing 2000 records in one and will throw a
    # 504 (timeout). So we keep retrying, each time halving the requested payload.
    rep <- 1
    max_tries <- 10

    # Repeat across 2000 records a time, reducing the payload where timeouts occur.
    repeat{
      cat(paste0("\nStarting page ", rep))
      attempt <- 1

      # While loop repeats request with payload halved each time until it succeeds
      # or reaches max_tries
      while(attempt <= max_tries){
        cat(paste0("\nAttempt ", attempt))
        cat(paste0("\nTrying with max records = ", max_records))
        response <- try(
          fetch_data(base_url = api_endpoint,
                     where_clause,
                     result_offset,
                     max_records)
        )

        if(!inherits(response, "try-error")){
          cat(paste0("\nSuccess with max records = ", max_records))
          break
        }

        attempt <- attempt + 1
        max_records <- max_records / 2 # try again by halving the payload
        cat("\nRetrying...")
      }

      geojson_data <- httr::content(response, as = "text")
      # Translate to sf object
      sf_chunk <- sf::st_read(geojson_data, quiet = TRUE)
      # Stop if no more records
      if (nrow(sf_chunk) == 0) break
      # Append results
      all_results <- append(all_results, list(sf_chunk))
      result_offset <- result_offset + max_records
      max_records <- 2000 # reset max_records
      rep <- rep + 1
    }
    t2 <- Sys.time()
    time_elapsed <- difftime(t2, t1, units = "secs")
    cat(paste0("\nRequest done in: ", time_elapsed, " seconds"))
    cat("\nNote time includes local data processing")

    msoa_geometries <- dplyr::bind_rows(all_results) %>%
      # Tidy up
      janitor::clean_names() %>%
      dplyr::select(msoa21cd, msoa21nm, shape_area, geometry)
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
    batch_size <- 90

    target_variable <- "MSOA21CD"

    chunks <- split(target_values, ceiling(seq_along(target_values) / batch_size))

    t1 <- Sys.time()
    cat("\nStarting request")
    for(chunk in chunks){
      # Update the where clause with the next batch
      where_clause <- paste0(target_variable, " IN ('", paste(chunk, collapse = "', '"), "')")

      # Run the query
      response <- fetch_data(base_url = api_endpoint,
                             where_clause,
                             result_offset, # redundant here but required for function
                             max_records) # redundant here but required for function

      # Get the contents
      geojson_data <- httr::content(response, as = "text")

      # Translate to sf object
      sf_chunk <- sf::st_read(geojson_data, quiet = TRUE)

      # Append results
      all_results <- append(all_results, list(sf_chunk))

    }
    t2 <- Sys.time()
    time_elapsed <- t2 - t1
    cat(paste0("\nRequest done in: ", round(time_elapsed,3), " seconds"))

    msoa_geometries <- dplyr::bind_rows(all_results) %>%
      # Tidy up
      janitor::clean_names() %>%
      dplyr::select(msoa21cd, msoa21nm, shape_area, geometry)
  }


  return(msoa_geometries)

}


get_lsoa_geometries <- function(subset = NULL){

  api_endpoint <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BFC_V10/FeatureServer/0/query"
  result_offset <- 0 # We may want the user to be able specify this
  max_records <- 2000 # We may want the user to be able specify this
  # Create cache directory - but ask user to agree
  policedatR::caching_check()
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  # Specify the request as a function so it can be memoised
  # Note we need to do something with the status code so that non-200 responses
  # don't get accepted and memoised. I've got a solution but not yet implemented
  fetch_data <- function(base_url,
                         where_clause,
                         result_offset,
                         max_records # GeoPortal max records in single response is 2000
  ) {
    params <- list(
      where = where_clause,  # Retrieve all records
      outFields =  "*", # "*" specifies all. I tried specifying fewer outfields but it doesn't speed things up. No need to specify geometry as this is assumed by default
      outSR = "4326",
      f = "geojson",
      resultOffset = result_offset, # for pagination
      resultRecordCount = max_records
    )

    # Initialise 504. Geoportal API saves 504 error to response content, so we
    # have to look there for the error. But let's only look when it seems likely
    # the request failed. we do this by using the length of the response - if it
    # looks really short, it's probably failed, so take a look and see if indeed
    # an error is reported
    error_504 <- FALSE

    response <- httr::GET(base_url, query = params)

    if(length(response[["content"]]) < 1000){
      peek_content <- httr::content(response, as = "text")
      if(grepl("error", peek_content, ignore.case = TRUE) && grepl("504", peek_content)){
        error_504 <- TRUE
        #max_records <- max_records / 2
      }
      else {
        error_504 <- FALSE
      }
    }
    if (httr::status_code(response) == 200 && error_504 == FALSE) { # add check in body for 504
      return(response)
      #content(response, "text")  # Convert response to text for caching
    }
    else {
      stop(paste("Error: Status code ", httr::status_code(response)," (but body may contain 504)"))
    }
  }

  if(caching){
    cd = cachem::cache_disk(cache_dir, evict = "lru")

    # Memoise the function
    fetch_data <- memoise::memoise(fetch_data, cache = cd)
  }

  # Initialise
  all_results <- list()
  # result_offset <- 0
  # max_records <- 2000

  # Subset options. Build on this later
  if(is.null(subset)){
    # If no subset has been requested, acquire all data
    # Here pagination is required as max records in single response is 2000
    where_clause <- "1=1" # we could build options here

    t1 <- Sys.time()
    cat("\nStarting request.")

    # Repeat the request in 2000 record batches
    # Sometimes the API doesn't like doing 2000 records in one and will throw a
    # 504 (timeout). So we keep retrying, each time halving the requested payload.
    rep <- 1
    max_tries <- 10

    # Repeat across 2000 records a time, reducing the payload where timeouts occur.
    repeat{
      cat(paste0("\nStarting page ", rep))
      attempt <- 1

      # While loop repeats request with payload halved each time until it succeeds
      # or reaches max_tries
      while(attempt <= max_tries){
        cat(paste0("\nAttempt ", attempt))
        cat(paste0("\nTrying with max records = ", max_records))
        response <- try(
          fetch_data(base_url = api_endpoint,
                     where_clause,
                     result_offset,
                     max_records)
        )

        if(!inherits(response, "try-error")){
          cat(paste0("\nSuccess with max records = ", max_records))
          break
        }

        attempt <- attempt + 1
        max_records <- max_records / 2 # try again by halving the payload
        cat("\nRetrying...")
      }

      geojson_data <- httr::content(response, as = "text")
      # Translate to sf object
      sf_chunk <- sf::st_read(geojson_data, quiet = TRUE)
      # Stop if no more records
      if (nrow(sf_chunk) == 0) break
      # Append results
      all_results <- append(all_results, list(sf_chunk))
      result_offset <- result_offset + max_records
      max_records <- 2000 # reset max_records
      rep <- rep + 1
    }
    t2 <- Sys.time()
    time_elapsed <- difftime(t2, t1, units = "secs")
    cat(paste0("\nRequest done in: ", round(time_elapsed,3), " seconds"))
    cat("\nNote time includes local data processing")

    lsoa_geometries <- dplyr::bind_rows(all_results) %>%
      # Tidy up
      janitor::clean_names() %>%
      dplyr::select(lsoa21cd, lsoa21nm, shape_area, geometry)
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
    batch_size <- 90

    target_variable <- "LSOA21CD"

    chunks <- split(target_values, ceiling(seq_along(target_values) / batch_size))

    t1 <- Sys.time()
    cat("\nStarting request")
    for(chunk in chunks){
      # Update the where clause with the next batch
      where_clause <- paste0(target_variable, " IN ('", paste(chunk, collapse = "', '"), "')")

      # Run the query
      response <- fetch_data(base_url = api_endpoint,
                             where_clause,
                             result_offset, # redundant here but function requires
                             max_records # redundant here but function requires
      )

      # Get the contents
      geojson_data <- httr::content(response, as = "text")

      # Translate to sf object
      sf_chunk <- sf::st_read(geojson_data, quiet = TRUE)

      # Append results
      all_results <- append(all_results, list(sf_chunk))

    }
    t2 <- Sys.time()
    time_elapsed <- t2 - t1
    cat(paste0("\nRequest done in: ", round(time_elapsed,3), " seconds"))

    lsoa_geometries <- dplyr::bind_rows(all_results) %>%
      # Tidy up
      janitor::clean_names() %>%
      dplyr::select(lsoa21cd, lsoa21nm, shape_area, geometry)
  }


  return(lsoa_geometries)

}




