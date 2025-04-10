#' Fetch police data
#'
#' Fetch police data workhorse for get_data functions. Submits POST requests to API
#' at data.police.uk. Can be memoised (and is in the get_data functions) to speed up repeat calls.
#'
#' In practice the user does not need to directly call this function.
#'
#' @param body Body of POST request. A named list consisting of two elements; a
#' 'poly' element defining a polygon as a character string in format 'lat1,long1:lat2,long2'
#' and a 'date' defining a date as character string in format 'yyyy-mm'.
#' @param wait_time On error, the time to wait before retrying. Default is 5.
#' @param max_tries The number of tries before giving up the call. Default is 10.
#'
#' @returns An http response object
#'
#' @export
#'
#' @examples
#'
#' # Simplified example where the polgyon is defined by only three points
#' fetch_police_data(body = list('poly' = "51.6094409248573,-0.127876986589682:51.6093817289251,-0.127378230001641:51.6093739825532,-0.12644271332808",
#'                             'date' = "2025-02"))
#'
fetch_police_data <- function(body,
                              wait_time = 5,
                              max_tries = 10){

  base_url <- 'https://data.police.uk/api/stops-street?'

  # search API for this coordinate set and date:
  response <- httr::POST(base_url, body = body)

  # if search quota reached, break (shouldn't be an issue but just in case)
  if(response[["status_code"]] == 429){
    print("Quota reached. Abandoning request.")
    break
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
      print(paste0("Max tries reached (", max_tries,")."))
      break
    }
  }
  return(response)
}
