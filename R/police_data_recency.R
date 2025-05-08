#' Retrieve most recent date for which data in API are available
#'
#'
#' @return A string describing date in yyyy-mm format
#' @export
#'
#' @examples
#'
#' newest_data()
#'
newest_data <- function(timeout = 10){

  newest <- tryCatch({
    R.utils::withTimeout({
      httr::GET("https://data.police.uk/api/crimes-street-dates", httr::timeout(timeout))
    }, timeout = timeout)  # Allow slightly longer than GET timeout, just in case
  }, TimeoutException = function(e) {
    message("Timed out")
    return(NULL)
  }, error = function(e) {
    message(e$message)
    return(NULL)
  })

  if(!is.null(newest)){
    if(httr::status_code(newest) == 200){
      k <- httr::content(newest)
      l <- lapply(k, unlist)
      m <- do.call(dplyr::bind_rows, l)
      m$date <- as.Date(paste0(m$date, "-01"))
      newest_date <- substr(max(m$date), 1, 7)
      message("Most recent month acquired from API")
    }
  }
  else{
    this_date <- Sys.Date()
    newest_date <- format(seq(this_date, length = 2, by = "-3 months")[2], "%Y-%m")
    message("Warning: API communication didn't work. Assuming most recent month is this month - 3.")
  }

  return(newest_date)

}

#' Retrieve oldest date for which data in API are available
#'
#'
#' @return A string describing date in yyyy-mm format
#' @export
#'
#' @examples
#'
#' oldest_data()
#'
#'
oldest_data <- function(timeout = 10){

  oldest <- tryCatch({
    R.utils::withTimeout({
      httr::GET("https://data.police.uk/api/crimes-street-dates", httr::timeout(timeout))
    }, timeout = timeout)  # Allow slightly longer than GET timeout, just in case
  }, TimeoutException = function(e) {
    message("Timed out")
    return(NULL)
  }, error = function(e) {
    message(e$message)
    return(NULL)
  })

  if(!is.null(oldest)){
    if(httr::status_code(oldest) == 200){
      k <- httr::content(oldest)
      l <- lapply(k, unlist)
      m <- do.call(dplyr::bind_rows, l)
      m$date <- as.Date(paste0(m$date, "-01"))
      oldest_date <- substr(min(m$date), 1, 7)
      message("Most recent month acquired from API")
    }
  }
  else{
    this_date <- Sys.Date()
    oldest_date <- format(seq(this_date, length = 2, by = "-39 months")[2], "%Y-%m")
    message("Warning: API communication didn't work. Assuming oldest month is this month - 39.")
  }

  return(oldest_date)

}
