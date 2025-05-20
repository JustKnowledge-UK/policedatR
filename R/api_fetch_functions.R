# Fetch police data
#
# Fetch police data workhorse for get_data functions. Submits POST requests to API
# at data.police.uk. Can be memoised (and is in the get_data functions) to speed up repeat calls.
#
# In practice the user does not need to directly call this function.
#
# @param body Body of POST request. A named list consisting of two elements; a
# 'poly' element defining a polygon as a character string in format 'lat1,long1:lat2,long2'
# and a 'date' defining a date as character string in format 'yyyy-mm'.
# @param wait_time On error, the time to wait before retrying. Default is 5.
# @param max_tries The number of tries before giving up the call. Default is 10.
#
# @returns An http response object
#
# @export
#
# @examples
#
# # Simplified example where the polygon is defined by only three points
# fetch_police_data(body = list(
#   'poly' = "51.6094409248573,-0.127876986589682:
#             51.6093817289251,-0.127378230001641:
#             51.6093739825532,-0.12644271332808",
#                            'date' = "2025-02"))
#
fetch_police_data <- function(body,
                              wait_time = 5,
                              max_tries = 10){

  base_url <- 'https://data.police.uk/api/stops-street?'

  # search API for this coordinate set and date:
  response <- httr::POST(base_url, body = body)

  # if search quota reached, break (shouldn't be an issue but just in case)
  if(response[["status_code"]] == 429){
    stop("Quota reached. Abandoning request.")

  }
  else{
    # if the request didn't succeed, wait some time ('wait_time') and
    # keep trying up until 'max_tries' attempts.
    attempt <- 1
    while(response[["status_code"]] != 200 && attempt <= max_tries){
      print(paste0("Server error: ", response[["status_code"]], ". Trying again (", attempt,")"))
      Sys.sleep(wait_time) # wait some time before trying again
      try(
        response <- httr::POST(base_url, body = body)
      )
      # if search quota reached, break (shouldn't be an issue but just in case)
      if(response[["status_code"]] == 429){
        print("Quota reached. Abandoning request.")
        break
      }
      attempt <- attempt + 1
    }

    # once max_tries is met, give up retry, save info including status code
    if(response[["status_code"]] != 200 && attempt > max_tries){
      stop(paste0("Max tries reached (", max_tries,")."))

    }
  }
  return(response)
}



# Fetch geometry data
#
# Fetch geometry data workhorse for get_geometry functions. Submits GET requests to API
# at geoportal.statistics.gov.uk. Can be memoised (and is in the get_geometry functions) to speed up repeat calls.
#
# In practice the user does not need to directly call this function.
#
# @param base_url The API endpoint. String.
# @param where_clause Subset of areas for which to acquire geometries. String
# @param result_offset For pagination user can start acquisition from an offset record number.
# @param max_records For pagination user can acquire a specific number of records. Geoportal max is 2000 but
# sometimes it prefers smaller payloads.
#
# @returns An http response object
# @export
#
# @examples
# \dontrun{
# # Don't run as takes a while
# # Simplified example where the polygon is defined by only three points
# response <- fetch_geometry_data(
#   base_url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Regions_December_2022_Boundaries_EN_BFC_V2/FeatureServer/0/query",
#   where_clause = "1=1",
#   result_offset = 0,
#   max_records = 2000
#   )
# }

fetch_geometry_data <- function(base_url,
                                where_clause,
                                result_offset = 0,
                                max_records = 2000 # GeoPortal max records in single response is 2000
                                ){
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

  return(response)
}


# Fetch population estimates
#
# API workhorse for `get_population_estimates()`. Communicates with NOMIS API to
# get population estimates by ethnicity for local areas specified by the smallest
# level of geography in data (which must be column 1). Population estimates are
# from Census 2021 table TS021.
#
# Can be (and is in `get_population_estimates()`) memoised for quicker repeat
# requests.
#
# @param data
#
# @returns An S3 response object containing the response of the request.
#
# @examples
#
# # Mock df
# df <- data.frame("lad22cd" = c("E09000001","E09000002","E09000003"),
#                  "x" = seq(1:3))
#
# response <- fetch_population_estimates(df)
#
fetch_population_estimates <- function(data){

  # Endpoint
  base <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_2041_1.csv?"
  date <- "date=latest"
  # Type of measure (value/percent)
  measures <- "measures=20100"
  # Ethnicity variable - this asks for all disaggregated and aggregated
  ethn <- "c2021_eth_20=0,1001,12,13,10,11,14,1002,16,15,17,1003,8,7,6,9,1004,1...5,1005,18,19"

  # Get areas as defined in data
  area_variable <- colnames(data)[1]
  if(area_variable == "pfa22cd"){
    pfas <- unique(data[,1])
    areas <- area_lookup %>%
      dplyr::select(lad22cd:pfa22nm) %>%
      dplyr::distinct() %>%
      dplyr::filter(!!rlang::sym(area_variable) %in% pfas) %>%
      dplyr::pull(lad22cd)

  }
  else{
    areas <- unique(data[,1])
  }

  geography <- paste0("geography=",paste(areas, collapse = ","))

  # Build the query
  query <- paste0(base, date,"&",geography,"&",ethn,"&",measures)

  # get the response
  response <- httr::GET(query)

  if (httr::status_code(response) != 200) {
    stop("Failed to fetch data from the API! (Non-200 status)")
  }

  return(response)

}



