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
    response <- fetch_data(where_clause)  # First call fetches from API
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



