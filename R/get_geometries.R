get_lad_geometries <- function(caching = TRUE){

  # API endpoint
  base_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Local_Authority_Districts_December_2022_UK_BFC_V2/FeatureServer/0/query"

  # response <- httr::GET(base_url, query = list(where = "1=1", outFields = "*", outSR = "4326", f = "geojson"))
  # Create cache directory - but ask user to agree
  cache_dir <- rappdirs::user_cache_dir(appname = NULL, appauthor = "policedatR")
  if(!dir.exists(cache_dir)){
    # repeat check until either y or n has been pressed
    repeat{
      check <- readline("We recommend caching the data acquired from geoportal.gov.uk
so that repeat queries can run faster. Do you want to create a local cache to save queries? (y/n)")
      if(check == "y"){
        print(paste0("Creating cache at ", cache_dir))
        dir.create(cache_dir, recursive = TRUE)
        break
      }
      else if(check == "n"){
        print("Not caching data. Repeat queries won't be quicker.")
        break
      }
      else{
        print("Please type either 'y' or 'n' and press Enter")
      }
    }
  }
  else {
    print(paste0("Cache detected. Files will be saved to ", cache_dir))
  }

  # Initialise and specify parameters for the query
  all_results <- list()
  where_clause <- "1=1" # we could build options here

  # Specify the request as a function so it can be memoised
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
    cd = cachem::cache_disk(cache_dir)
    # Memoise the function
    fetch_data <- memoise::memoise(fetch_data, cache = cd)
  }

  t1 <- Sys.time()
  print("Starting request")
  response <- fetch_data(where_clause)  # First call fetches from API
  t2 <- Sys.time()
  time_elapsed <- t2 - t1
  print(paste0("Request done in: ", time_elapsed, " seconds"))

  geojson_data <- httr::content(response, as = "text")
  # Translate to sf object
  lad_geometries <- sf::st_read(geojson_data, quiet = TRUE) %>%
    # Tidy up
    janitor::clean_names() %>%
    select(lad22cd, lad22nm, shape_area, geometry)

  return(lad_geometries)

}



