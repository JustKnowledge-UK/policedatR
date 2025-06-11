#' Get stop and search data for Regions
#'
#' Acquire stop and search data from police.uk API for Regions
#'
#' @param subset A named list defining the areas for which data are required. Names
#' correspond to the area variable on which to subset. Values correspond to the desired
#' values of the area variable. See `area_variables()` for areas that can be used to subset.
#'
#' If the areas specified are at a lower level of geography, the Region to which the lower geography belongs will be returned.
#'
#' If NULL will acquire for all areas.
#'
#' @param num_months_backwards The number of months backwards from the start
#' point for which to acquire data. Default is 12. Maximum is 36 (limit imposed by API).
#' @param oldest_month Instead of specifying `num_months_backwards`, the user
#' can specify the oldest month of interest (in combination with the oldest year
#' of interest). Numeric value in format M (e.g., 8 for August).
#' @param oldest_year Instead of specifying `num_months_backwards`, the user
#' can specify the oldest year of interest (in combination with the oldest
#' month of interest). Numeric value in format YYYY (e.g., 2019).
#' @param most_recent_month The most recent month of interest. Numeric value in format M
#' (e.g., 8 for August). By default the function will determine this based on
#' the most recent API update.
#' @param most_recent_year The most recent month of interest. Numeric value in format YYYY
#' (e.g., 2021). By default the function will determine this based on the most
#' recent API update.
#' @param wait_time If there is a server error when submitting POST request,
#' the function will wait `wait_time` seconds before retrying. Numeric value.
#' Default is 5.
#' @param max_tries Specify the maximum number of times to retry a failed
#' POST request. Numeric value. Default is 10. Failures are usually due to
#' timeouts, which are often resolved by retrying, but can also be because there
#' is no data avaible for the area/date combination.
#' @param include_no_stop_areas Whether to include areas for which there are no results.
#' Default is TRUE (recommended as this will flag areas for which there are no stops and so
#' provide most comprehensive picture).
#'
#' @returns A data frame where each row is a separate stop and search record.
#' @export
#'
#' @examples
#'
#'\dontrun{
#' # Get the most recent 12 months of data for all Regions.
#' # Don't run as will take a while.
#' df <- get_region_data()
#'
#' }
#' # Get data for a subset of Regions, only the most recent month
#' df2 <- get_region_data(subset = list("rgn22nm" = "London"), num_months_backwards = 1)
#'
#'
#' # Get data for the Region to which Haringey belongs (i.e. London). This
#' # will give identical result to example 2 above.
#' df3 <- get_region_data(subset = list("lad22nm" = c("Haringey")), num_months_backwards = 1)
#'
#'
get_region_data <- function(subset = NULL,
                         num_months_backwards = 12,
                         oldest_month = NULL,
                         oldest_year = NULL,
                         most_recent_month = NULL,
                         most_recent_year = NULL,
                         wait_time = 5,
                         max_tries = 5,
                         include_no_stop_areas = TRUE,
                         cache = TRUE
                         #n_records = 3 # For testing
){

  policedatR::caching_check(cache)
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  #### Error handling ####


  # Make sure that user can't specify an incorrect time period
  # 1. Too many months backwards
  if(!is.null(num_months_backwards)){
    if(num_months_backwards > 36){
      stop("Error in num_months_backwards. The API only holds data for the previous 36 months. Respecify num_months_backwards")
    }
  }

  # 2. Back to year/month specification exceeds 36 months backwards
  oldest_date <- as.Date(paste0(oldest_data(),"-01"))
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    if(as.Date(paste0(oldest_year,"-",oldest_month,"-01")) < oldest_date){
      stop("Error in oldest_month/oldest_year. The API only holds data for the previous 36 months. The oldest date is ", substr(oldest_date, 1, 7))
    }
  }

  # 3. Most recent specification is too recent
  newest_date <- as.Date(paste0(newest_data(),"-01"))
  if(!is.null(most_recent_month) && !is.null(most_recent_year)){
    if(as.Date(paste0(most_recent_year,"-", most_recent_month,"-01")) > newest_date){
      stop("Error in most_recent_month/most_recent_year. The most recent data available is ", substr(newest_date, 1, 7))
    }
  }

  # 4. Date not fully specified
  ## one of the 'oldest' arguments missing
  if((!is.null(oldest_month) && is.null(oldest_year)) ||
     (is.null(oldest_month) && !is.null(oldest_year))){
    stop("Error: Both oldest_month and oldest_year must be specified.")
  }
  ## one of the 'most_recent' arguments missing
  if((!is.null(most_recent_month) && is.null(most_recent_year)) ||
     (is.null(most_recent_month) && !is.null(most_recent_year))){
    stop("Error: Both most_recent_month and most_recent_year must be specified.")
  }
  ## most recent specified with no target in the past (with default setting for num_months_backwards this won't happen)
  if(!is.null(most_recent_month) && !is.null(most_recent_year) &&
     is.null(num_months_backwards) &&
     (is.null(oldest_month) || is.null(oldest_year))){
    stop("Error: Time period not specified or incomplete. Either num_months_backwards or both oldest_month and oldest_year must be specified")
  }


  #### End: Error handling ####


  t1 <- Sys.time()


  #### Get geometries ####


  geometries <- policedatR::get_region_geometries(subset)


  #### End: Get geometries ####


  # Initialise variables
  overall_output <- data.frame()
  # no_entries_df <- data.frame(setNames(rep(list(NA), 6), c("Index", "Name","County","Region","Country","Force")))
  # server_error_df <- data.frame(setNames(rep(list(NA), 10), c("Index","Name","County","Region","Country","Force","Date", "Date_Index", "Coordinate_Set_Index", "Status_Code")))

  # get most recent update if to data not specified
  if(is.null(most_recent_month) || is.null(most_recent_year)){
    # get most recent update from API:
    date <- httr::content(
      httr::GET("https://data.police.uk/api/crimes-street-dates"))[[1]][["date"]]
    most_recent_month <- as.numeric(substr(date,6,7))
    most_recent_year <- as.numeric(substr(date,1,4))
  }
  else{
    most_recent_month <- most_recent_month
    most_recent_year <- most_recent_year
  }

  # if oldest_month/year has been specified, redefine num_months_backwards
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    num_months_backwards <- 1 + (12 * (most_recent_year - oldest_year)) + (most_recent_month - oldest_month)
  }
  else{
    num_months_backwards <- num_months_backwards
  }

  # For testing - subset to smaller number of geometries
  # if(!is.null(n_records)){
  #   # only adjust geometries if it n_records is smaller than nrow(geometries)
  #   if(n_records < nrow(geometries)){
  #     geometries <- geometries[1:n_records,]
  #   }
  # }


  #### h loop: iterates over areas ####


  for(h in 1:nrow(geometries)){
    cat(paste0("\nStarted area ", h)) # report start (useful for debugging)

    area_output <- data.frame() # initialise area output df
    number_months_acquired <- 0 # initialise number of months acquired counter


    #### i loop: iterates over the months required ####


    for(i in 1:num_months_backwards){
      month_output <- data.frame()

      # format date to what is needed for API query ("yyyy-mm")
      if(i == 1){ # set values for first iteration
        month_num <- most_recent_month
        year <- most_recent_year
      }
      else{ # subsequent iterations
        month_num <- month_num - 1 # backwards a month each iteration
        if(month_num %% 12 == 0){ # if reach a new year, start months from 12 again
          month_num <- 12
          year <- year - 1 # backwards a year
        }
      }
      if(month_num < 10){ # paste 0 for months lower than 10
        month <- paste("0", month_num, sep = "")
      }
      else{
        month <- month_num
      }

      date <- paste(year, "-", month, sep = "") # combine dates into one string


      #### j loop ####

      # Iterates over the coordinate sets within each LA and creates a
      # polygon string to be searched, and then submits the query.
      # Most LAs have only one coordinate set, but some have multiple (e.g.,
      # those that include islands). The function therefore needs to search
      # each coordinate set within a LA separately.

      # Identify the column number for geometry
      geometry_column_number <- which(colnames(geometries) == "geometry")

      # Initialise vector for coord string
      coord_string <- c()

      for(j in 1:length(geometries[[geometry_column_number]][[h]])){

        # set this iteration's coordinate set
        # We must differentiate between polygons and multipolygons. When choosing
        # subsets it is possible the result will be polygon (where the area is just
        # polygon rather than multiple). If this is the case geometry is one list
        # less nested than the multipolygon case
        area_coords <- if(
          # if the hth, jth element of the geometry column is a list, go a level deeper
          # otherwise just pick the hth, jth elemen
          is.list(geometries[[geometry_column_number]][[h]][[j]])){
          geometries[[geometry_column_number]][[h]][[j]][[1]]
        } else {
          geometries[[geometry_column_number]][[h]][[j]]
        }

        area_coords <- area_coords %>%
          tibble::as_tibble(.name_repair = ~c("long","lat"))
        # long is e.g. -1.13
        # lat is e.g. 52.62

        # Create an areas df with no geometry column as placeholder in case no
        # data found for this area. This is so we can still count areas where
        # no data was found. It's also to provide area info for lower geography
        # cases (not relevant for LADs)
        areas_df <- geometries[h,] %>%
          sf::st_drop_geometry()

        # combine coord strings into format required by API (much quicker than looping)
        coord_string <- paste0(area_coords$lat,",",area_coords$long, collapse = ":")

        # create body for post request
        body <- list("poly" = coord_string,
                     "date" = date)

        if(caching){
          cd = cachem::cache_disk(cache_dir, evict = "lru")
          # Memoise the function
          fetch_police_data_memoised <- memoise::memoise(fetch_police_data, cache = cd)
          post_request <- fetch_police_data_memoised(body = body)
        }
        else {
          post_request <- fetch_police_data(body = body)
        }


        # get data from results of query
        df <- httr::content(post_request)

        # unlist data and convert to dataframe
        df_2 <- lapply(df, unlist)
        df_3 <- do.call(dplyr::bind_rows, df_2)
        df_3$coord_set <- j # record which coordinate set data is from

        # add results of this coordinate set iteration (j) to the output for
        # this month iteration (i). Use bindrows for (high) possibility that columns
        # from different iterations will be in different order/missing
        month_output <- dplyr::bind_rows(month_output, df_3)

        cat("\014") # clear console
        # report overall (i.e., LA) progress
        cat(paste0("Working... Area ", h, " of ", nrow(geometries),
                   " (",
                   round(100 * (h / nrow(geometries)), 2), "%)"))
        # report month progress
        cat(paste0("\nWorking... Month ", i, " of ", num_months_backwards,
                   " (", date, ")"))
        # report coordinate set progress
        cat(paste0("\nWorking... ", j, " of ",
                   length(geometries[[geometry_column_number]][[h]]),
                   " coordinate sets retrieved"))

      }


      #### End: Coordinate set loop (j) ####




      # If records have been acquired, increase months count
      if(nrow(month_output) > 0){
        number_months_acquired <- number_months_acquired + 1
      }

      # Add data from this month (i) to overall area output (h)
      area_output <- dplyr::bind_rows(area_output, month_output)

    }


    #### End: Month loop (i) ####


    # If there were no records for this LA, record the LA iteration number
    # and name, then proceed to the next LA
    # if(nrow(area_output) == 0){
    #   if(is.na(no_entries_df[1,1])){ # if first occurrence, replaces NAs
    #     no_entries_df[1,] <- c(h, la_name, county, region, country, force)
    #   }
    #   else{ # rbind subsequent occurrences
    #     no_entries_df <- rbind(no_entries_df, c(h, la_name, county, region, country, force))
    #   }
    #   print(paste0("No records for ", la_name))
    #   next # proceed to next LA
    # }
    # print(nrow(area_output))
    # print(area_output)

    # If the area output is empty, no stops were found in the area.
    # where this is the case, report the area data, with NAs for the rest
    # and explicitly tag these areas as having no stops with stops_found = FALSE
    if(include_no_stop_areas==TRUE){
      if(nrow(area_output) == 0){
        area_output <- areas_df %>% # add in all the area data by binding the columns for this area
          dplyr::mutate(
            stops_found = FALSE
          )
      } else{
        # if stops were found for this area, add in the areas to the front
        # and tag the rows with stops_found = TRUE
        area_output <- dplyr::bind_cols(areas_df, area_output) %>%
          dplyr::mutate(
            stops_found = TRUE
          )
      }
    }

    # if(nrow(area_output==0)){
    #   area_output <- bind_rows(area_output, as.data.frame(matrix(NA, ncol = ncol(area_output), nrow = 1)))
    # }

    # add columns for LA name, county, region, country, and the iteration index
    # for the LA (useful for quickly identifying which LA the function reached
    # if it breaks unexpectedly)
    # area_output$la_name <- la_name
    # area_output$la_code <- la_code
    # area_output$county <- county
    # area_output$region <- region
    # area_output$country <- country
    # area_output$index <- h
    # area_output$number_months_acquired <- number_months_acquired
    # area_output$proportion_months_acquired <- number_months_acquired / num_months_backwards
    # area_output$force <- force
    #
    # # separate datetime into 2 columns
    # area_output$time <- substr(area_output$datetime, 12, 19)
    # area_output$date <- as.Date(substr(area_output$datetime, 1, 10))
    # # set as time
    # #area_output$time <- chron(times. =  df$time, format = c(times = "hh:mm:ss"))
    # save(area_output, file = "test.Rdata")


    # add records from this area (h) to overall output
    overall_output <- dplyr::bind_rows(overall_output, area_output)

    # create a temporary output list and save it every time a LA completes, so
    # that there is a backup in case function breaks. Saves to same folder as
    # script
    # save_progress <- list(result = overall_output,
    #                       missing_entries = no_entries_df,
    #                       server_errors = server_error_df,
    #                       last_area_acquired = h)
    # save(save_progress, file = "./save_progress.Rdata")
  }


  #### End: Area loop (h) ####


  # Add in area lookup. Not much to do here for regions
  # areas_above <- area_lookup %>%
  #   dplyr::select(c(pfa22cd:rgn22nm)) %>%
  #   dplyr::distinct()

  # If datetime exists in overall_output (and therefore stops were found),
  # relocate it. Otherwise don't try to relocate datetime (as it isn't there!)
  if("datetime" %in% colnames(overall_output)){
    overall_output <- overall_output %>%
      dplyr::relocate(datetime, .after = rgn22nm)
  }


  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units = "secs")
  cat(paste0("\nDone in ",round(time_elapsed, 3), " seconds"))

  return(overall_output)

}

#' Get stop and search data for Police Force Areas
#'
#' Acquire stop and search data from police.uk API for Police Force Areas (PFAs)
#'
#' @param subset A named list defining the areas for which data are required. Names
#' correspond to the area variable on which to subset. Values correspond to the desired
#' values of the area variable. See `area_variables()` for areas that can be used to subset.
#'
#' If the areas specified are at a higher level of geography, all PFAs within the higher geographies will be returned.
#' If the areas specified are at a lower level of geography, the PFAs to which the lower geography belongs will be returned.
#'
#' If NULL will acquire for all areas.
#' @param num_months_backwards The number of months backwards from the start
#' point for which to acquire data. Default is 12. Maximum is 36 (limit imposed by API).
#' @param oldest_month Instead of specifying `num_months_backwards`, the user
#' can specify the oldest month of interest (in combination with the oldest year
#' of interest). Numeric value in format M (e.g., 8 for August).
#' @param oldest_year Instead of specifying `num_months_backwards`, the user
#' can specify the oldest year of interest (in combination with the oldest
#' month of interest). Numeric value in format YYYY (e.g., 2019).
#' @param most_recent_month The most recent month of interest. Numeric value in format M
#' (e.g., 8 for August). By default the function will determine this based on
#' the most recent API update.
#' @param most_recent_year The most recent month of interest. Numeric value in format YYYY
#' (e.g., 2021). By default the function will determine this based on the most
#' recent API update.
#' @param wait_time If there is a server error when submitting POST request,
#' the function will wait `wait_time` seconds before retrying. Numeric value.
#' Default is 5.
#' @param max_tries Specify the maximum number of times to retry a failed
#' POST request. Numeric value. Default is 10. Failures are usually due to
#' timeouts, which are often resolved by retrying, but can also be because there
#' is no data avaible for the area/date combination.
#' @param include_no_stop_areas Whether to include areas for which there are no results.
#' Default is TRUE (recommended as this will flag areas for which there are no stops and so
#' provide most comprehensive picture).
#'
#' @returns A data frame where each row is a separate stop and search record.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get the most recent 12 months of data for all PFAs.
#' # Don't run as will take a while
#' df <- get_pfa_data()
#' }
#'
#' # Get data for a subset of PFAs, only the most recent month.
#' df2 <- get_pfa_data(subset = list("pfa22nm" = "Metropolitan Police"), num_months_backwards = 1)
#'
#' # Get data for the PFA to which Haringey belongs (i.e. Metropolitan Police). This
#' # will give identical result to example 2 above.
#' df3 <- get_pfa_data(subset = list("lad22nm" = c("Haringey")), num_months_backwards = 1)
#'
#'
get_pfa_data <- function(subset = NULL,
                         num_months_backwards = 12,
                         oldest_month = NULL,
                         oldest_year = NULL,
                         most_recent_month = NULL,
                         most_recent_year = NULL,
                         wait_time = 5,
                         max_tries = 5,
                         include_no_stop_areas = TRUE,
                         cache = TRUE
                         #n_records = 3 # For testing
){

  policedatR::caching_check(cache)
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")


  #### Error handling ####


  # Make sure that user can't specify an incorrect time period
  # 1. Too many months backwards
  if(!is.null(num_months_backwards)){
    if(num_months_backwards > 36){
      stop("Error in num_months_backwards. The API only holds data for the previous 36 months. Respecify num_months_backwards")
    }
  }

  # 2. Back to year/month specification exceeds 36 months backwards
  oldest_date <- as.Date(paste0(oldest_data(),"-01"))
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    if(as.Date(paste0(oldest_year,"-",oldest_month,"-01")) < oldest_date){
      stop("Error in oldest_month/oldest_year. The API only holds data for the previous 36 months. The oldest date is ", substr(oldest_date, 1, 7))
    }
  }

  # 3. Most recent specification is too recent
  newest_date <- as.Date(paste0(newest_data(),"-01"))
  if(!is.null(most_recent_month) && !is.null(most_recent_year)){
    if(as.Date(paste0(most_recent_year,"-", most_recent_month,"-01")) > newest_date){
      stop("Error in most_recent_month/most_recent_year. The most recent data available is ", substr(newest_date, 1, 7))
    }
  }

  # 4. Date not fully specified
  ## one of the 'oldest' arguments missing
  if((!is.null(oldest_month) && is.null(oldest_year)) ||
     (is.null(oldest_month) && !is.null(oldest_year))){
    stop("Error: Both oldest_month and oldest_year must be specified.")
  }
  ## one of the 'most_recent' arguments missing
  if((!is.null(most_recent_month) && is.null(most_recent_year)) ||
     (is.null(most_recent_month) && !is.null(most_recent_year))){
    stop("Error: Both most_recent_month and most_recent_year must be specified.")
  }
  ## most recent specified with no target in the past (with default setting for num_months_backwards this won't happen)
  if(!is.null(most_recent_month) && !is.null(most_recent_year) &&
     is.null(num_months_backwards) &&
     (is.null(oldest_month) || is.null(oldest_year))){
    stop("Error: Time period not specified or incomplete. Either num_months_backwards or both oldest_month and oldest_year must be specified")
  }



  t1 <- Sys.time()


  #### Get geometries ####

  geometries <- policedatR::get_pfa_geometries(subset)



  # Initialise variables
  overall_output <- data.frame()
  # no_entries_df <- data.frame(setNames(rep(list(NA), 6), c("Index", "Name","County","Region","Country","Force")))
  # server_error_df <- data.frame(setNames(rep(list(NA), 10), c("Index","Name","County","Region","Country","Force","Date", "Date_Index", "Coordinate_Set_Index", "Status_Code")))

  # get most recent update if to data not specified
  if(is.null(most_recent_month) || is.null(most_recent_year)){
    # get most recent update from API:
    date <- httr::content(
      httr::GET("https://data.police.uk/api/crimes-street-dates"))[[1]][["date"]]
    most_recent_month <- as.numeric(substr(date,6,7))
    most_recent_year <- as.numeric(substr(date,1,4))
  }
  else{
    most_recent_month <- most_recent_month
    most_recent_year <- most_recent_year
  }

  # if oldest_month/year has been specified, redefine num_months_backwards
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    num_months_backwards <- 1 + (12 * (most_recent_year - oldest_year)) + (most_recent_month - oldest_month)
  }
  else{
    num_months_backwards <- num_months_backwards
  }

  # For testing - subset to smaller number of geometries
  # if(!is.null(n_records)){
  #   # only adjust geometries if it n_records is smaller than nrow(geometries)
  #   if(n_records < nrow(geometries)){
  #     geometries <- geometries[1:n_records,]
  #   }
  # }


  #### h loop: iterates over areas ####


  for(h in 1:nrow(geometries)){
    cat(paste0("\nStarted area ", h)) # report start (useful for debugging)

    area_output <- data.frame() # initialise area output df
    number_months_acquired <- 0 # initialise number of months acquired counter


    #### i loop: iterates over the months required ####


    for(i in 1:num_months_backwards){
      month_output <- data.frame()

      # format date to what is needed for API query ("yyyy-mm")
      if(i == 1){ # set values for first iteration
        month_num <- most_recent_month
        year <- most_recent_year
      }
      else{ # subsequent iterations
        month_num <- month_num - 1 # backwards a month each iteration
        if(month_num %% 12 == 0){ # if reach a new year, start months from 12 again
          month_num <- 12
          year <- year - 1 # backwards a year
        }
      }
      if(month_num < 10){ # paste 0 for months lower than 10
        month <- paste("0", month_num, sep = "")
      }
      else{
        month <- month_num
      }

      date <- paste(year, "-", month, sep = "") # combine dates into one string


      #### j loop ####

      # Iterates over the coordinate sets within each LA and creates a
      # polygon string to be searched, and then submits the query.
      # Most LAs have only one coordinate set, but some have multiple (e.g.,
      # those that include islands). The function therefore needs to search
      # each coordinate set within a LA separately.

      # Identify the column number for geometry
      geometry_column_number <- which(colnames(geometries) == "geometry")

      # Initialise vector for coord string
      coord_string <- c()

      for(j in 1:length(geometries[[geometry_column_number]][[h]])){

        # set this iteration's coordinate set
        # We must differentiate between polygons and multipolygons. When choosing
        # subsets it is possible the result will be polygon (where the area is just
        # polygon rather than multiple). If this is the case geometry is one list
        # less nested than the multipolygon case
        area_coords <- if(
          # if the hth, jth element of the geometry column is a list, go a level deeper
          # otherwise just pick the hth, jth elemen
          is.list(geometries[[geometry_column_number]][[h]][[j]])){
          geometries[[geometry_column_number]][[h]][[j]][[1]]
        } else {
          geometries[[geometry_column_number]][[h]][[j]]
        }

        area_coords <- area_coords %>%
          tibble::as_tibble(.name_repair = ~c("long","lat"))
        # long is e.g. -1.13
        # lat is e.g. 52.62

        # Create an areas df with no geometry column as placeholder in case no
        # data found for this area. This is so we can still count areas where
        # no data was found. It's also to provide area info for lower geography
        # cases (not relevant for LADs)
        areas_df <- geometries[h,] %>%
          sf::st_drop_geometry()

        # combine coord strings into format required by API (much quicker than looping)
        coord_string <- paste0(area_coords$lat,",",area_coords$long, collapse = ":")

        # create body for post request
        body <- list("poly" = coord_string,
                     "date" = date)

        if(caching){
          cd = cachem::cache_disk(cache_dir, evict = "lru")
          # Memoise the function
          fetch_police_data_memoised <- memoise::memoise(fetch_police_data, cache = cd)
          post_request <- fetch_police_data_memoised(body = body)
        }
        else {
          post_request <- fetch_police_data(body = body)
        }

        # get data from results of query
        df <- httr::content(post_request)

        # unlist data and convert to dataframe
        df_2 <- lapply(df, unlist)
        df_3 <- do.call(dplyr::bind_rows, df_2)
        df_3$coord_set <- j # record which coordinate set data is from

        # add results of this coordinate set iteration (j) to the output for
        # this month iteration (i). Use bindrows for (high) possibility that columns
        # from different iterations will be in different order/missing
        month_output <- dplyr::bind_rows(month_output, df_3)

        cat("\014") # clear console
        # report overall (i.e., LA) progress
        cat(paste0("Working... Area ", h, " of ", nrow(geometries),
                   " (",
                   round(100 * (h / nrow(geometries)), 2), "%)"))
        # report month progress
        cat(paste0("\nWorking... Month ", i, " of ", num_months_backwards,
                   " (", date, ")"))
        # report coordinate set progress
        cat(paste0("\nWorking... ", j, " of ",
                   length(geometries[[geometry_column_number]][[h]]),
                   " coordinate sets retrieved"))

      }


      #### End: Coordinate set loop (j) ####




      # If records have been acquired, increase months count
      if(nrow(month_output) > 0){
        number_months_acquired <- number_months_acquired + 1
      }

      # Add data from this month (i) to overall area output (h)
      area_output <- dplyr::bind_rows(area_output, month_output)

    }


    #### End: Month loop (i) ####


    # If there were no records for this LA, record the LA iteration number
    # and name, then proceed to the next LA
    # if(nrow(area_output) == 0){
    #   if(is.na(no_entries_df[1,1])){ # if first occurrence, replaces NAs
    #     no_entries_df[1,] <- c(h, la_name, county, region, country, force)
    #   }
    #   else{ # rbind subsequent occurrences
    #     no_entries_df <- rbind(no_entries_df, c(h, la_name, county, region, country, force))
    #   }
    #   print(paste0("No records for ", la_name))
    #   next # proceed to next LA
    # }
    # print(nrow(area_output))
    # print(area_output)

    # If the area output is empty, no stops were found in the area.
    # where this is the case, report the area data, with NAs for the rest
    # and explicitly tag these areas as having no stops with stops_found = FALSE
    if(include_no_stop_areas==TRUE){
      if(nrow(area_output) == 0){
        area_output <- areas_df %>% # add in all the area data by binding the columns for this area
          dplyr::mutate(
            stops_found = FALSE
          )
      } else{
        # if stops were found for this area, add in the areas to the front
        # and tag the rows with stops_found = TRUE
        area_output <- dplyr::bind_cols(areas_df, area_output) %>%
          dplyr::mutate(
            stops_found = TRUE
          )
      }
    }


    # if(nrow(area_output==0)){
    #   area_output <- bind_rows(area_output, as.data.frame(matrix(NA, ncol = ncol(area_output), nrow = 1)))
    # }

    # add columns for LA name, county, region, country, and the iteration index
    # for the LA (useful for quickly identifying which LA the function reached
    # if it breaks unexpectedly)
    # area_output$la_name <- la_name
    # area_output$la_code <- la_code
    # area_output$county <- county
    # area_output$region <- region
    # area_output$country <- country
    # area_output$index <- h
    # area_output$number_months_acquired <- number_months_acquired
    # area_output$proportion_months_acquired <- number_months_acquired / num_months_backwards
    # area_output$force <- force
    #
    # # separate datetime into 2 columns
    # area_output$time <- substr(area_output$datetime, 12, 19)
    # area_output$date <- as.Date(substr(area_output$datetime, 1, 10))
    # # set as time
    # #area_output$time <- chron(times. =  df$time, format = c(times = "hh:mm:ss"))
    # save(area_output, file = "test.Rdata")


    # add records from this area (h) to overall output
    overall_output <- dplyr::bind_rows(overall_output, area_output)

    # create a temporary output list and save it every time a LA completes, so
    # that there is a backup in case function breaks. Saves to same folder as
    # script
    # save_progress <- list(result = overall_output,
    #                       missing_entries = no_entries_df,
    #                       server_errors = server_error_df,
    #                       last_area_acquired = h)
    # save(save_progress, file = "./save_progress.Rdata")
  }


  #### End: Area loop (h) ####


  # Add in area lookup
  # First just get pfas and above from lookup
  areas_above <- area_lookup %>%
    dplyr::select(c(pfa22cd:rgn22nm)) %>%
    dplyr::distinct()

  # Then join
  overall_output <- overall_output %>%
    dplyr::left_join(areas_above, by = c("pfa22cd", "pfa22nm")) %>%
    # Locate the area variables at the left of the dataframe
    dplyr::relocate(c(rgn22cd:rgn22nm), .after = pfa22nm)

  # If datetime exists in overall_output (and therefore stops were found),
  # relocate it. Otherwise don't try to relocate datetime (as it isn't there!)
  if("datetime" %in% colnames(overall_output)){
    overall_output <- overall_output %>%
      dplyr::relocate(datetime, .after = rgn22nm)
  }

  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units = "secs")
  cat(paste0("\nDone in ",round(time_elapsed, 3), " seconds"))

  return(overall_output)

}

#' Get stop and search data for Local Authority Districts
#'
#' Acquire stop and search data from police.uk API for  Local Authority Districts (LADs)
#'
#' @param subset A named list defining the areas for which data are required. Names
#' correspond to the area variable on which to subset. Values correspond to the desired
#' values of the area variable. See `area_variables()` for areas that can be used to subset.
#' If NULL will acquire for all areas. Note this will take a while. Consider chunking
#' a large number of desired areas into separate calls.
#' @param num_months_backwards The number of months backwards from the start
#' point for which to acquire data. Default is 12. Maximum is 36 (limit imposed by API).
#' @param oldest_month Instead of specifying `num_months_backwards`, the user
#' can specify the oldest month of interest (in combination with the oldest year
#' of interest). Numeric value in format M (e.g., 8 for August).
#' @param oldest_year Instead of specifying `num_months_backwards`, the user
#' can specify the oldest year of interest (in combination with the oldest
#' month of interest). Numeric value in format YYYY (e.g., 2019).
#' @param most_recent_month The most recent month of interest. Numeric value in format M
#' (e.g., 8 for August). By default the function will determine this based on
#' the most recent API update.
#' @param most_recent_year The most recent month of interest. Numeric value in format YYYY
#' (e.g., 2021). By default the function will determine this based on the most
#' recent API update.
#' @param wait_time If there is a server error when submitting POST request,
#' the function will wait `wait_time` seconds before retrying. Numeric value.
#' Default is 5.
#' @param max_tries Specify the maximum number of times to retry a failed
#' POST request. Numeric value. Default is 10. Failures are usually due to
#' timeouts, which are often resolved by retrying, but can also be because there
#' is no data avaible for the area/date combination.
#' @param include_no_stop_areas Whether to include areas for which there are no results.
#' Default is TRUE (recommended as this will flag areas for which there are no stops and so
#' provide most comprehensive picture).
#'
#' @returns A data frame where each row is a separate stop and search record.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get the most recent 12 months of data for all LADs.
#' # Don't run as will take a while.
#' df <- get_lad_data()
#'}
#' # Get data for a subset of LADs, only the most recent month
#' df2 <- get_lad_data(subset = list(
#'                       "lad22nm" = c('Haringey','Waltham Forest')),
#'                       num_months_backwards = 1)
#'
#'
get_lad_data <- function(subset = NULL,
                         num_months_backwards = 12,
                         oldest_month = NULL,
                         oldest_year = NULL,
                         most_recent_month = NULL,
                         most_recent_year = NULL,
                         wait_time = 5,
                         max_tries = 5,
                         include_no_stop_areas = TRUE,
                         cache = TRUE
                         #n_records = 3 # For testing
                         ){

  policedatR::caching_check(cache)
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")


  #### Error handling ####


  # Make sure that user can't specify an incorrect time period
  # 1. Too many months backwards
  if(!is.null(num_months_backwards)){
    if(num_months_backwards > 36){
      stop("Error in num_months_backwards. The API only holds data for the previous 36 months. Respecify num_months_backwards")
    }
  }

  # 2. Back to year/month specification exceeds 36 months backwards
  oldest_date <- as.Date(paste0(oldest_data(),"-01"))
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    if(as.Date(paste0(oldest_year,"-",oldest_month,"-01")) < oldest_date){
      stop("Error in oldest_month/oldest_year. The API only holds data for the previous 36 months. The oldest date is ", substr(oldest_date, 1, 7))
    }
  }

  # 3. Most recent specification is too recent
  newest_date <- as.Date(paste0(newest_data(),"-01"))
  if(!is.null(most_recent_month) && !is.null(most_recent_year)){
    if(as.Date(paste0(most_recent_year,"-", most_recent_month,"-01")) > newest_date){
      stop("Error in most_recent_month/most_recent_year. The most recent data available is ", substr(newest_date, 1, 7))
    }
  }

  # 4. Date not fully specified
  ## one of the 'oldest' arguments missing
  if((!is.null(oldest_month) && is.null(oldest_year)) ||
     (is.null(oldest_month) && !is.null(oldest_year))){
    stop("Error: Both oldest_month and oldest_year must be specified.")
  }
  ## one of the 'most_recent' arguments missing
  if((!is.null(most_recent_month) && is.null(most_recent_year)) ||
     (is.null(most_recent_month) && !is.null(most_recent_year))){
    stop("Error: Both most_recent_month and most_recent_year must be specified.")
  }
  ## most recent specified with no target in the past (with default setting for num_months_backwards this won't happen)
  if(!is.null(most_recent_month) && !is.null(most_recent_year) &&
     is.null(num_months_backwards) &&
     (is.null(oldest_month) || is.null(oldest_year))){
    stop("Error: Time period not specified or incomplete. Either num_months_backwards or both oldest_month and oldest_year must be specified")
  }


  t1 <- Sys.time()


  #### Get geometries ####


  geometries <- policedatR::get_lad_geometries(subset)



  # Initialise variables
  overall_output <- data.frame()
  # no_entries_df <- data.frame(setNames(rep(list(NA), 6), c("Index", "Name","County","Region","Country","Force")))
  # server_error_df <- data.frame(setNames(rep(list(NA), 10), c("Index","Name","County","Region","Country","Force","Date", "Date_Index", "Coordinate_Set_Index", "Status_Code")))

  # get most recent update if to data not specified
  if(is.null(most_recent_month) || is.null(most_recent_year)){
    # get most recent update from API:
    date <- httr::content(
      httr::GET("https://data.police.uk/api/crimes-street-dates"))[[1]][["date"]]
    most_recent_month <- as.numeric(substr(date,6,7))
    most_recent_year <- as.numeric(substr(date,1,4))
  }
  else{
    most_recent_month <- most_recent_month
    most_recent_year <- most_recent_year
  }

  # if oldest_month/year has been specified, redefine num_months_backwards
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    num_months_backwards <- 1 + (12 * (most_recent_year - oldest_year)) + (most_recent_month - oldest_month)
  }
  else{
    num_months_backwards <- num_months_backwards
  }

  # For testing - subset to smaller number of geometries
  # if(!is.null(n_records)){
  #   # only adjust geometries if it n_records is smaller than nrow(geometries)
  #   if(n_records < nrow(geometries)){
  #     geometries <- geometries[1:n_records,]
  #   }
  # }


  #### h loop: iterates over areas ####


  for(h in 1:nrow(geometries)){
    cat(paste0("\nStarted area ", h)) # report start (useful for debugging)

    area_output <- data.frame() # initialise area output df
    number_months_acquired <- 0 # initialise number of months acquired counter


    #### i loop: iterates over the months required ####


    for(i in 1:num_months_backwards){
      month_output <- data.frame()

      # format date to what is needed for API query ("yyyy-mm")
      if(i == 1){ # set values for first iteration
        month_num <- most_recent_month
        year <- most_recent_year
      }
      else{ # subsequent iterations
        month_num <- month_num - 1 # backwards a month each iteration
        if(month_num %% 12 == 0){ # if reach a new year, start months from 12 again
          month_num <- 12
          year <- year - 1 # backwards a year
        }
      }
      if(month_num < 10){ # paste 0 for months lower than 10
        month <- paste("0", month_num, sep = "")
      }
      else{
        month <- month_num
      }

      date <- paste(year, "-", month, sep = "") # combine dates into one string


      #### j loop ####

      # Iterates over the coordinate sets within each LA and creates a
      # polygon string to be searched, and then submits the query.
      # Most LAs have only one coordinate set, but some have multiple (e.g.,
      # those that include islands). The function therefore needs to search
      # each coordinate set within a LA separately.

      # Identify the column number for geometry
      geometry_column_number <- which(colnames(geometries) == "geometry")

      # Initialise vector for coord string
      coord_string <- c()

      for(j in 1:length(geometries[[geometry_column_number]][[h]])){

        # set this iteration's coordinate set
        # We must differentiate between polygons and multipolygons. When choosing
        # subsets it is possible the result will be polygon (where the area is just
        # polygon rather than multiple). If this is the case geometry is one list
        # less nested than the multipolygon case
        area_coords <- if(
          # if the hth, jth element of the geometry column is a list, go a level deeper
          # otherwise just pick the hth, jth elemen
            is.list(geometries[[geometry_column_number]][[h]][[j]])){
              geometries[[geometry_column_number]][[h]][[j]][[1]]
          } else {
            geometries[[geometry_column_number]][[h]][[j]]
            }

        area_coords <- area_coords %>%
          tibble::as_tibble(.name_repair = ~c("long","lat"))
        # long is e.g. -1.13
        # lat is e.g. 52.62

        # Create an areas df with no geometry column as placeholder in case no
        # data found for this area. This is so we can still count areas where
        # no data was found. It's also to provide area info for lower geography
        # cases (not relevant for LADs)
        areas_df <- geometries[h,] %>%
          sf::st_drop_geometry()

        # combine coord strings into format required by API (much quicker than looping)
        coord_string <- paste0(area_coords$lat,",",area_coords$long, collapse = ":")

        # create body for post request
        body <- list("poly" = coord_string,
                     "date" = date)

        if(caching){
          cd = cachem::cache_disk(cache_dir, evict = "lru")
          # Memoise the function
          fetch_police_data_memoised <- memoise::memoise(fetch_police_data, cache = cd)
          post_request <- fetch_police_data_memoised(body = body)
        }
        else {
          post_request <- fetch_police_data(body = body)
        }

        # get data from results of query
        df <- httr::content(post_request)

        # unlist data and convert to dataframe
        df_2 <- lapply(df, unlist)
        df_3 <- do.call(dplyr::bind_rows, df_2)
        df_3$coord_set <- j # record which coordinate set data is from

        # add results of this coordinate set iteration (j) to the output for
        # this month iteration (i). Use bindrows for (high) possibility that columns
        # from different iterations will be in different order/missing
        month_output <- dplyr::bind_rows(month_output, df_3)

        cat("\014") # clear console
        # report overall (i.e., LA) progress
        cat(paste0("Working... Area ", h, " of ", nrow(geometries),
                     " (",
                     round(100 * (h / nrow(geometries)), 2), "%)"))
        # report month progress
        cat(paste0("\nWorking... Month ", i, " of ", num_months_backwards,
                     " (", date, ")"))
        # report coordinate set progress
        cat(paste0("\nWorking... ", j, " of ",
                     length(geometries[[geometry_column_number]][[h]]),
                     " coordinate sets retrieved"))

      }


      #### End: Coordinate set loop (j) ####




      # If records have been acquired, increase months count
      if(nrow(month_output) > 0){
        number_months_acquired <- number_months_acquired + 1
      }

      # Add data from this month (i) to overall area output (h)
      area_output <- dplyr::bind_rows(area_output, month_output)

    }


    #### End: Month loop (i) ####


    # If there were no records for this LA, record the LA iteration number
    # and name, then proceed to the next LA
    # if(nrow(area_output) == 0){
    #   if(is.na(no_entries_df[1,1])){ # if first occurrence, replaces NAs
    #     no_entries_df[1,] <- c(h, la_name, county, region, country, force)
    #   }
    #   else{ # rbind subsequent occurrences
    #     no_entries_df <- rbind(no_entries_df, c(h, la_name, county, region, country, force))
    #   }
    #   print(paste0("No records for ", la_name))
    #   next # proceed to next LA
    # }
    # print(nrow(area_output))
    # print(area_output)

    # If the area output is empty, no stops were found in the area.
    # where this is the case, report the area data, with NAs for the rest
    # and explicitly tag these areas as having no stops with stops_found = FALSE
    if(include_no_stop_areas==TRUE){
      if(nrow(area_output) == 0){
        area_output <- areas_df %>% # add in all the area data by binding the columns for this area
          dplyr::mutate(
            stops_found = FALSE
          )
      } else{
        # if stops were found for this area, add in the areas to the front
        # and tag the rows with stops_found = TRUE
        area_output <- dplyr::bind_cols(areas_df, area_output) %>%
          dplyr::mutate(
            stops_found = TRUE
          )
      }
    }

    # if(nrow(area_output==0)){
    #   area_output <- bind_rows(area_output, as.data.frame(matrix(NA, ncol = ncol(area_output), nrow = 1)))
    # }

    # add columns for LA name, county, region, country, and the iteration index
    # for the LA (useful for quickly identifying which LA the function reached
    # if it breaks unexpectedly)
    # area_output$la_name <- la_name
    # area_output$la_code <- la_code
    # area_output$county <- county
    # area_output$region <- region
    # area_output$country <- country
    # area_output$index <- h
    # area_output$number_months_acquired <- number_months_acquired
    # area_output$proportion_months_acquired <- number_months_acquired / num_months_backwards
    # area_output$force <- force
    #
    # # separate datetime into 2 columns
    # area_output$time <- substr(area_output$datetime, 12, 19)
    # area_output$date <- as.Date(substr(area_output$datetime, 1, 10))
    # # set as time
    # #area_output$time <- chron(times. =  df$time, format = c(times = "hh:mm:ss"))
    # save(area_output, file = "test.Rdata")


    # add records from this area (h) to overall output
    overall_output <- dplyr::bind_rows(overall_output, area_output)

    # create a temporary output list and save it every time a LA completes, so
    # that there is a backup in case function breaks. Saves to same folder as
    # script
    # save_progress <- list(result = overall_output,
    #                       missing_entries = no_entries_df,
    #                       server_errors = server_error_df,
    #                       last_area_acquired = h)
    # save(save_progress, file = "./save_progress.Rdata")
  }


  #### End: Area loop (h) ####


  # Add in area lookup
  # First just get msoas and above from lookup
  areas_above <- area_lookup %>%
    dplyr::select(c(lad22cd:rgn22nm)) %>%
    dplyr::distinct()

  # Then join
  overall_output <- overall_output %>%
    dplyr::left_join(areas_above, by = c("lad22cd", "lad22nm")) %>%
    # Locate the area variables at the left of the dataframe
    dplyr::relocate(c(pfa22cd:rgn22nm), .after = lad22nm)

  # If datetime exists in overall_output (and therefore stops were found),
  # relocate it. Otherwise don't try to relocate datetime (as it isn't there!)
  if("datetime" %in% colnames(overall_output)){
    overall_output <- overall_output %>%
      dplyr::relocate(datetime, .after = rgn22nm)
  }


  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units = "secs")
  cat(paste0("\nDone in ",round(time_elapsed, 3), " seconds"))

  return(overall_output)

}


#' Get stop and search data for Middle layer Super Output Areas
#'
#' Acquire stop and search data from police.uk API for Middle layer Super Output Areas (MSOAs).
#'
#' @param subset A named list defining the areas for which data are required. Names
#' correspond to the area variable on which to subset. Values correspond to the desired
#' values of the area variable. See `area_variables()` for areas that can be used to subset.
#' If NULL will acquire for all areas. Note this will take a long time. Consider chunking
#' a large number of desired areas into separate calls.
#' @param num_months_backwards The number of months backwards from the start
#' point for which to acquire data. Default is 12. Maximum is 36 (limit imposed by API).
#' @param oldest_month Instead of specifying `num_months_backwards`, the user
#' can specify the oldest month of interest (in combination with the oldest year
#' of interest). Numeric value in format M (e.g., 8 for August).
#' @param oldest_year Instead of specifying `num_months_backwards`, the user
#' can specify the oldest year of interest (in combination with the oldest
#' month of interest). Numeric value in format YYYY (e.g., 2019).
#' @param most_recent_month The most recent month of interest. Numeric value in format M
#' (e.g., 8 for August). By default the function will determine this based on
#' the most recent API update.
#' @param most_recent_year The most recent month of interest. Numeric value in format YYYY
#' (e.g., 2021). By default the function will determine this based on the most
#' recent API update.
#' @param wait_time If there is a server error when submitting POST request,
#' the function will wait `wait_time` seconds before retrying. Numeric value.
#' Default is 5.
#' @param max_tries Specify the maximum number of times to retry a failed
#' POST request. Numeric value. Default is 10. Failures are usually due to
#' timeouts, which are often resolved by retrying, but can also be because there
#' is no data avaible for the area/date combination.
#' @param include_no_stop_areas Whether to include areas for which there are no results.
#' Default is TRUE (recommended as this will flag areas for which there are no stops and so
#' provide most comprehensive picture).
#'
#' @returns A data frame where each row is a separate stop and search record.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get the most recent 12 months of data for all MSOAs.
#' Don't run as will take a long time.
#' df <- get_msoa_data()
#'
#' # Get data for a subset of MSOAs, only the last 6 months
#' # Don't run as will take ~17 seconds.
#' df2 <- get_msoa_data(subset = list("lad22nm" = 'Haringey'), num_months_backwards = 1)
#' }
#'

get_msoa_data <- function(subset = NULL,
                         num_months_backwards = 12,
                         oldest_month = NULL,
                         oldest_year = NULL,
                         most_recent_month = NULL,
                         most_recent_year = NULL,
                         wait_time = 5,
                         max_tries = 5,
                         include_no_stop_areas = TRUE,
                         cache = TRUE
                         # n_records = NULL # For testing
){

  policedatR::caching_check(cache)
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  #### Error handling ####


  # Make sure that user can't specify an incorrect time period
  # 1. Too many months backwards
  if(!is.null(num_months_backwards)){
    if(num_months_backwards > 36){
      stop("Error in num_months_backwards. The API only holds data for the previous 36 months. Respecify num_months_backwards")
    }
  }

  # 2. Back to year/month specification exceeds 36 months backwards
  oldest_date <- as.Date(paste0(oldest_data(),"-01"))
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    if(as.Date(paste0(oldest_year,"-",oldest_month,"-01")) < oldest_date){
      stop("Error in oldest_month/oldest_year. The API only holds data for the previous 36 months. The oldest date is ", substr(oldest_date, 1, 7))
    }
  }

  # 3. Most recent specification is too recent
  newest_date <- as.Date(paste0(newest_data(),"-01"))
  if(!is.null(most_recent_month) && !is.null(most_recent_year)){
    if(as.Date(paste0(most_recent_year,"-", most_recent_month,"-01")) > newest_date){
      stop("Error in most_recent_month/most_recent_year. The most recent data available is ", substr(newest_date, 1, 7))
    }
  }

  # 4. Date not fully specified
  ## one of the 'oldest' arguments missing
  if((!is.null(oldest_month) && is.null(oldest_year)) ||
     (is.null(oldest_month) && !is.null(oldest_year))){
    stop("Error: Both oldest_month and oldest_year must be specified.")
  }
  ## one of the 'most_recent' arguments missing
  if((!is.null(most_recent_month) && is.null(most_recent_year)) ||
     (is.null(most_recent_month) && !is.null(most_recent_year))){
    stop("Error: Both most_recent_month and most_recent_year must be specified.")
  }
  ## most recent specified with no target in the past (with default setting for num_months_backwards this won't happen)
  if(!is.null(most_recent_month) && !is.null(most_recent_year) &&
     is.null(num_months_backwards) &&
     (is.null(oldest_month) || is.null(oldest_year))){
    stop("Error: Time period not specified or incomplete. Either num_months_backwards or both oldest_month and oldest_year must be specified")
  }


  t1 <- Sys.time()


  #### Get geometries ####

  geometries <- policedatR::get_msoa_geometries(subset)



  # Initialise variables
  overall_output <- data.frame()
  # no_entries_df <- data.frame(setNames(rep(list(NA), 6), c("Index", "Name","County","Region","Country","Force")))
  # server_error_df <- data.frame(setNames(rep(list(NA), 10), c("Index","Name","County","Region","Country","Force","Date", "Date_Index", "Coordinate_Set_Index", "Status_Code")))

  # get most recent update if to data not specified
  if(is.null(most_recent_month) || is.null(most_recent_year)){
    # get most recent update from API:
    date <- httr::content(
      httr::GET("https://data.police.uk/api/crimes-street-dates"))[[1]][["date"]]
    most_recent_month <- as.numeric(substr(date,6,7))
    most_recent_year <- as.numeric(substr(date,1,4))
  }
  else{
    most_recent_month <- most_recent_month
    most_recent_year <- most_recent_year
  }

  # if oldest_month/year has been specified, redefine num_months_backwards
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    num_months_backwards <- 1 + (12 * (most_recent_year - oldest_year)) + (most_recent_month - oldest_month)
  }
  else{
    num_months_backwards <- num_months_backwards
  }

  # For testing - subset to smaller number of geometries
  # if(!is.null(n_records)){
  #   # only adjust geometries if n_records is smaller than nrow(geometries)
  #   if(n_records < nrow(geometries)){
  #     geometries <- geometries[1:n_records,]
  #   }
  # }




  #### h loop: iterates over areas ####


  for(h in 1:nrow(geometries)){
    cat(paste0("\nStarted area ", h)) # report start (useful for debugging)

    area_output <- data.frame() # initialise area output df
    number_months_acquired <- 0 # initialise number of months acquired counter

    #### i loop: iterates over the months required ####

    for(i in 1:num_months_backwards){
      month_output <- data.frame()

      # format date to what is needed for API query ("yyyy-mm")
      if(i == 1){ # set values for first iteration
        month_num <- most_recent_month
        year <- most_recent_year
      }
      else{ # subsequent iterations
        month_num <- month_num - 1 # backwards a month each iteration
        if(month_num %% 12 == 0){ # if reach a new year, start months from 12 again
          month_num <- 12
          year <- year - 1 # backwards a year
        }
      }
      if(month_num < 10){ # paste 0 for months lower than 10
        month <- paste("0", month_num, sep = "")
      }
      else{
        month <- month_num
      }

      date <- paste(year, "-", month, sep = "") # combine dates into one string

      #### j loop ####
      # Iterates over the coordinate sets within each LA and creates a
      # polygon string to be searched, and then submits the query.
      # Most LAs have only one coordinate set, but some have multiple (e.g.,
      # those that include islands). The function therefore needs to search
      # each coordinate set within a LA separately.

      # Identify the column number for geometry
      geometry_column_number <- which(colnames(geometries) == "geometry")

      # Initialise vector for coord string
      coord_string <- c()

      for(j in 1:length(geometries[[geometry_column_number]][[h]])){

        # set this iteration's coordinate set
        # We must differentiate between polygons and multipolygons. When choosing
        # subsets it is possible the result will be polygon (where the area is just
        # polygon rather than multiple). If this is the case geometry is one list
        # less nested than the multipolygon case
        area_coords <- if(
          # if the hth, jth element of the geometry column is a list, go a level deeper
          # otherwise just pick the hth, jth elemen
          is.list(geometries[[geometry_column_number]][[h]][[j]])){
          geometries[[geometry_column_number]][[h]][[j]][[1]]
        } else {
          geometries[[geometry_column_number]][[h]][[j]]
        }

        area_coords <- area_coords %>%
          tibble::as_tibble(.name_repair = ~c("long","lat"))
        # long is e.g. -1.13
        # lat is e.g. 52.62

        # Create an areas df with no geometry column as placeholder in case no
        # data found for this area. This is so we can still count areas where
        # no data was found. It's also to provide area info for lower geography
        # cases (not relevant for LADs)
        areas_df <- geometries[h,] %>%
          sf::st_drop_geometry()

        # combine coord strings into format required by API (much quicker than looping)
        coord_string <- paste0(area_coords$lat,",",area_coords$long, collapse = ":")

        # create body for post request
        body <- list("poly" = coord_string,
                     "date" = date)

        if(caching){
          cd = cachem::cache_disk(cache_dir, evict = "lru")
          # Memoise the function
          fetch_police_data_memoised <- memoise::memoise(fetch_police_data, cache = cd)
          post_request <- fetch_police_data_memoised(body = body)
        }
        else {
          post_request <- fetch_police_data(body = body)
        }


        # get data from results of query
        df <- httr::content(post_request)

        # unlist data and convert to dataframe
        df_2 <- lapply(df, unlist)
        df_3 <- do.call(dplyr::bind_rows, df_2)
        df_3$coord_set <- j # record which coordinate set data is from

        # add results of this coordinate set iteration (j) to the output for
        # this month iteration (i). Use bindrows for (high) possibility that columns
        # from different iterations will be in different order/missing
        month_output <- dplyr::bind_rows(month_output, df_3)

        cat("\014") # clear console
        # report overall (i.e., LA) progress
        cat(paste0("Working... Area ", h, " of ", nrow(geometries),
                   " (",
                   round(100 * (h / nrow(geometries)), 2), "%)"))
        # report month progress
        cat(paste0("\nWorking... Month ", i, " of ", num_months_backwards,
                   " (", date, ")"))
        # report coordinate set progress
        cat(paste0("\nWorking... ", j, " of ",
                   length(geometries[[geometry_column_number]][[h]]),
                   " coordinate sets retrieved"))

      }

      #### End: Coordinate set loop (j) ####



      # If records have been acquired, increase months count
      if(nrow(month_output) > 0){
        number_months_acquired <- number_months_acquired + 1
      }

      # Add data from this month (i) to overall area output (h)
      area_output <- dplyr::bind_rows(area_output, month_output)

    }

    #### End: Month loop (i) ####

    # If there were no records for this LA, record the LA iteration number
    # and name, then proceed to the next LA
    # if(nrow(area_output) == 0){
    #   if(is.na(no_entries_df[1,1])){ # if first occurrence, replaces NAs
    #     no_entries_df[1,] <- c(h, la_name, county, region, country, force)
    #   }
    #   else{ # rbind subsequent occurrences
    #     no_entries_df <- rbind(no_entries_df, c(h, la_name, county, region, country, force))
    #   }
    #   print(paste0("No records for ", la_name))
    #   next # proceed to next LA
    # }
    # print(nrow(area_output))
    # print(area_output)

    # If the area output is empty, no stops were found in the area.
    # where this is the case, report the area data, with NAs for the rest
    # and explicitly tag these areas as having no stops with stops_found = FALSE
    if(include_no_stop_areas==TRUE){
      if(nrow(area_output) == 0){
        area_output <- areas_df %>% # add in all the area data by binding the columns for this area
          dplyr::mutate(
            stops_found = FALSE
          )
      } else{
        # if stops were found for this area, add in the areas to the front
        # and tag the rows with stops_found = TRUE
        area_output <- dplyr::bind_cols(areas_df, area_output) %>%
          dplyr::mutate(
            stops_found = TRUE
          )
      }
    }


    # if(nrow(area_output==0)){
    #   area_output <- bind_rows(area_output, as.data.frame(matrix(NA, ncol = ncol(area_output), nrow = 1)))
    # }

    # add columns for LA name, county, region, country, and the iteration index
    # for the LA (useful for quickly identifying which LA the function reached
    # if it breaks unexpectedly)
    # area_output$la_name <- la_name
    # area_output$la_code <- la_code
    # area_output$county <- county
    # area_output$region <- region
    # area_output$country <- country
    # area_output$index <- h
    # area_output$number_months_acquired <- number_months_acquired
    # area_output$proportion_months_acquired <- number_months_acquired / num_months_backwards
    # area_output$force <- force
    #
    # # separate datetime into 2 columns
    # area_output$time <- substr(area_output$datetime, 12, 19)
    # area_output$date <- as.Date(substr(area_output$datetime, 1, 10))
    # # set as time
    # #area_output$time <- chron(times. =  df$time, format = c(times = "hh:mm:ss"))
    # save(area_output, file = "test.Rdata")


    # add records from this area (h) to overall output
    overall_output <- dplyr::bind_rows(overall_output, area_output)

    # create a temporary output list and save it every time a LA completes, so
    # that there is a backup in case function breaks. Saves to same folder as
    # script
    # save_progress <- list(result = overall_output,
    #                       missing_entries = no_entries_df,
    #                       server_errors = server_error_df,
    #                       last_area_acquired = h)
    # save(save_progress, file = "./save_progress.Rdata")
  }


  #### End: Area loop (h) ####

  # Add in area lookup
  # First just get msoas and above from lookup
  areas_above <- area_lookup %>%
    dplyr::select(c(msoa21cd:rgn22nm)) %>%
    dplyr::distinct()

  # Then join
  overall_output <- overall_output %>%
    dplyr::left_join(areas_above, by = c("msoa21cd", "msoa21nm")) %>%
    # Locate the area variables at the left of the dataframe
    dplyr::relocate(c(lad22cd:rgn22nm), .after = msoa21nm)

  # If datetime exists in overall_output (and therefore stops were found),
  # relocate it. Otherwise don't try to relocate datetime (as it isn't there!)
  if("datetime" %in% colnames(overall_output)){
    overall_output <- overall_output %>%
      dplyr::relocate(datetime, .after = rgn22nm)
  }

  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units = "secs")
  cat(paste0("\nDone in ",round(time_elapsed, 3), " seconds"))

  return(overall_output)

}

# Get LSOA data

#' Get stop and search data for Lower layer Super Output Areas
#'
#' Acquire stop and search data from police.uk API for Lower layer Super Output Areas (LSOAs).
#'
#' @param subset A named list defining the areas for which data are required. Names
#' correspond to the area variable on which to subset. Values correspond to the desired
#' values of the area variable. See `area_variables()` for areas that can be used to subset.
#' If NULL will acquire for all areas. Note this will take a very long time. Consider chunking
#' a large number of desired areas into separate calls.
#'
#' @param num_months_backwards The number of months backwards from the start
#' point for which to acquire data. Default is 12. Maximum is 36 (limit imposed by API).
#' @param oldest_month Instead of specifying `num_months_backwards`, the user
#' can specify the oldest month of interest (in combination with the oldest year
#' of interest). Numeric value in format M (e.g., 8 for August).
#' @param oldest_year Instead of specifying `num_months_backwards`, the user
#' can specify the oldest year of interest (in combination with the oldest
#' month of interest). Numeric value in format YYYY (e.g., 2019).
#' @param most_recent_month The most recent month of interest. Numeric value in format M
#' (e.g., 8 for August). By default the function will determine this based on
#' the most recent API update.
#' @param most_recent_year The most recent month of interest. Numeric value in format YYYY
#' (e.g., 2021). By default the function will determine this based on the most
#' recent API update.
#' @param wait_time If there is a server error when submitting POST request,
#' the function will wait `wait_time` seconds before retrying. Numeric value.
#' Default is 5.
#' @param max_tries Specify the maximum number of times to retry a failed
#' POST request. Numeric value. Default is 10. Failures are usually due to
#' timeouts, which are often resolved by retrying, but can also be because there
#' is no data avaible for the area/date combination.
#' @param include_no_stop_areas Whether to include areas for which there are no results.
#' Default is TRUE (recommended as this will flag areas for which there are no stops and so
#' provide most comprehensive picture).
#'
#' @returns A data frame where each row is a separate stop and search record.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get the most recent 12 months of data for all LSOAs. This will take a long time!
#' # Don't run as will take a long time
#' df <- get_lsoa_data()
#'
#' # Get data for a subset of MSOAs, only the last 6 months
#' # Don't run as will take ~52 seconds.
#' df2 <- get_lsoa_data(subset = list("lad22nm" = 'Haringey'), num_months_backwards = 1)
#' }
#'
#'
#'
get_lsoa_data <- function(subset = NULL,
                          num_months_backwards = 12,
                          oldest_month = NULL,
                          oldest_year = NULL,
                          most_recent_month = NULL,
                          most_recent_year = NULL,
                          wait_time = 5,
                          max_tries = 5,
                          include_no_stop_areas = TRUE,
                          cache = TRUE
                          # n_records = NULL # For testing
){


  policedatR::caching_check(cache)
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  #### Error handling ####

  # Make sure that user can't specify an incorrect time period
  # 1. Too many months backwards
  if(!is.null(num_months_backwards)){
    if(num_months_backwards > 36){
      stop("Error in num_months_backwards. The API only holds data for the previous 36 months. Respecify num_months_backwards")
    }
  }

  # 2. Back to year/month specification exceeds 36 months backwards
  oldest_date <- as.Date(paste0(oldest_data(),"-01"))
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    if(as.Date(paste0(oldest_year,"-",oldest_month,"-01")) < oldest_date){
      stop("Error in oldest_month/oldest_year. The API only holds data for the previous 36 months. The oldest date is ", substr(oldest_date, 1, 7))
    }
  }

  # 3. Most recent specification is too recent
  newest_date <- as.Date(paste0(newest_data(),"-01"))
  if(!is.null(most_recent_month) && !is.null(most_recent_year)){
    if(as.Date(paste0(most_recent_year,"-", most_recent_month,"-01")) > newest_date){
      stop("Error in most_recent_month/most_recent_year. The most recent data available is ", substr(newest_date, 1, 7))
    }
  }

  # 4. Date not fully specified
  ## one of the 'oldest' arguments missing
  if((!is.null(oldest_month) && is.null(oldest_year)) ||
     (is.null(oldest_month) && !is.null(oldest_year))){
    stop("Error: Both oldest_month and oldest_year must be specified.")
  }
  ## one of the 'most_recent' arguments missing
  if((!is.null(most_recent_month) && is.null(most_recent_year)) ||
     (is.null(most_recent_month) && !is.null(most_recent_year))){
    stop("Error: Both most_recent_month and most_recent_year must be specified.")
  }
  ## most recent specified with no target in the past (with default setting for num_months_backwards this won't happen)
  if(!is.null(most_recent_month) && !is.null(most_recent_year) &&
     is.null(num_months_backwards) &&
     (is.null(oldest_month) || is.null(oldest_year))){
    stop("Error: Time period not specified or incomplete. Either num_months_backwards or both oldest_month and oldest_year must be specified")
  }



  t1 <- Sys.time()

  #### Get geometries ####

  geometries <- policedatR::get_lsoa_geometries(subset)

  # Initialise variables
  overall_output <- data.frame()
  # no_entries_df <- data.frame(setNames(rep(list(NA), 6), c("Index", "Name","County","Region","Country","Force")))
  # server_error_df <- data.frame(setNames(rep(list(NA), 10), c("Index","Name","County","Region","Country","Force","Date", "Date_Index", "Coordinate_Set_Index", "Status_Code")))

  # get most recent update if to data not specified
  if(is.null(most_recent_month) || is.null(most_recent_year)){
    # get most recent update from API:
    date <- httr::content(
      httr::GET("https://data.police.uk/api/crimes-street-dates"))[[1]][["date"]]
    most_recent_month <- as.numeric(substr(date,6,7))
    most_recent_year <- as.numeric(substr(date,1,4))
  }
  else{
    most_recent_month <- most_recent_month
    most_recent_year <- most_recent_year
  }

  # if oldest_month/year has been specified, redefine num_months_backwards
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    num_months_backwards <- 1 + (12 * (most_recent_year - oldest_year)) + (most_recent_month - oldest_month)
  }
  else{
    num_months_backwards <- num_months_backwards
  }

  # For testing - subset to smaller number of geometries
  # if(!is.null(n_records)){
  #   # only adjust geometries if n_records is smaller than nrow(geometries)
  #   if(n_records < nrow(geometries)){
  #     geometries <- geometries[1:n_records,]
  #   }
  # }



  #### h loop: iterates over areas ####

  for(h in 1:nrow(geometries)){
    cat(paste0("\nStarted area ", h)) # report start (useful for debugging)

    area_output <- data.frame() # initialise area output df
    number_months_acquired <- 0 # initialise number of months acquired counter

    #### i loop: iterates over the months required ####

    for(i in 1:num_months_backwards){
      month_output <- data.frame()

      # format date to what is needed for API query ("yyyy-mm")
      if(i == 1){ # set values for first iteration
        month_num <- most_recent_month
        year <- most_recent_year
      }
      else{ # subsequent iterations
        month_num <- month_num - 1 # backwards a month each iteration
        if(month_num %% 12 == 0){ # if reach a new year, start months from 12 again
          month_num <- 12
          year <- year - 1 # backwards a year
        }
      }
      if(month_num < 10){ # paste 0 for months lower than 10
        month <- paste("0", month_num, sep = "")
      }
      else{
        month <- month_num
      }

      date <- paste(year, "-", month, sep = "") # combine dates into one string

      #### j loop ####
      # Iterates over the coordinate sets within each LA and creates a
      # polygon string to be searched, and then submits the query.
      # Most LAs have only one coordinate set, but some have multiple (e.g.,
      # those that include islands). The function therefore needs to search
      # each coordinate set within a LA separately.

      # Identify the column number for geometry
      geometry_column_number <- which(colnames(geometries) == "geometry")

      # Initialise vector for coord string
      coord_string <- c()

      for(j in 1:length(geometries[[geometry_column_number]][[h]])){

        # set this iteration's coordinate set
        # We must differentiate between polygons and multipolygons. When choosing
        # subsets it is possible the result will be polygon (where the area is just
        # polygon rather than multiple). If this is the case geometry is one list
        # less nested than the multipolygon case
        area_coords <- if(
          # if the hth, jth element of the geometry column is a list, go a level deeper
          # otherwise just pick the hth, jth elemen
          is.list(geometries[[geometry_column_number]][[h]][[j]])){
          geometries[[geometry_column_number]][[h]][[j]][[1]]
        } else {
          geometries[[geometry_column_number]][[h]][[j]]
        }

        area_coords <- area_coords %>%
          tibble::as_tibble(.name_repair = ~c("long","lat"))
        # long is e.g. -1.13
        # lat is e.g. 52.62

        # Create an areas df with no geometry column as placeholder in case no
        # data found for this area. This is so we can still count areas where
        # no data was found. It's also to provide area info for lower geography
        # cases (not relevant for LADs)
        areas_df <- geometries[h,] %>%
          sf::st_drop_geometry()

        # combine coord strings into format required by API (much quicker than looping)
        coord_string <- paste0(area_coords$lat,",",area_coords$long, collapse = ":")

        # create body for post request
        body <- list("poly" = coord_string,
                     "date" = date)

        if(caching){
          cd = cachem::cache_disk(cache_dir, evict = "lru")
          # Memoise the function
          fetch_police_data_memoised <- memoise::memoise(fetch_police_data, cache = cd)
          post_request <- fetch_police_data_memoised(body = body)
        }
        else {
          post_request <- fetch_police_data(body = body)
        }


        # get data from results of query
        df <- httr::content(post_request)

        # unlist data and convert to dataframe
        df_2 <- lapply(df, unlist)
        df_3 <- do.call(dplyr::bind_rows, df_2)
        df_3$coord_set <- j # record which coordinate set data is from

        # add results of this coordinate set iteration (j) to the output for
        # this month iteration (i). Use bindrows for (high) possibility that columns
        # from different iterations will be in different order/missing
        month_output <- dplyr::bind_rows(month_output, df_3)

        cat("\014") # clear console
        # report overall (i.e., LA) progress
        cat(paste0("Working... Area ", h, " of ", nrow(geometries),
                   " (",
                   round(100 * (h / nrow(geometries)), 2), "%)"))
        # report month progress
        cat(paste0("\nWorking... Month ", i, " of ", num_months_backwards,
                   " (", date, ")"))
        # report coordinate set progress
        cat(paste0("\nWorking... ", j, " of ",
                   length(geometries[[geometry_column_number]][[h]]),
                   " coordinate sets retrieved"))

      }

      #### End: Coordinate set loop (j) ####


      # If records have been acquired, increase months count
      if(nrow(month_output) > 0){
        number_months_acquired <- number_months_acquired + 1
      }

      # Add data from this month (i) to overall area output (h)
      area_output <- dplyr::bind_rows(area_output, month_output)

    }

    #### End: Month loop (i) ####

    # If there were no records for this LA, record the LA iteration number
    # and name, then proceed to the next LA
    # if(nrow(area_output) == 0){
    #   if(is.na(no_entries_df[1,1])){ # if first occurrence, replaces NAs
    #     no_entries_df[1,] <- c(h, la_name, county, region, country, force)
    #   }
    #   else{ # rbind subsequent occurrences
    #     no_entries_df <- rbind(no_entries_df, c(h, la_name, county, region, country, force))
    #   }
    #   print(paste0("No records for ", la_name))
    #   next # proceed to next LA
    # }
    # print(nrow(area_output))
    # print(area_output)

    # If the area output is empty, no stops were found in the area.
    # where this is the case, report the area data, with NAs for the rest
    # and explicitly tag these areas as having no stops with stops_found = FALSE
    if(include_no_stop_areas==TRUE){
      if(nrow(area_output) == 0){
        area_output <- areas_df %>% # add in all the area data by binding the columns for this area
          dplyr::mutate(
            stops_found = FALSE
          )
      } else{
        # if stops were found for this area, add in the areas to the front
        # and tag the rows with stops_found = TRUE
        area_output <- dplyr::bind_cols(areas_df, area_output) %>%
          dplyr::mutate(
            stops_found = TRUE
          )
      }
    }


    # if(nrow(area_output==0)){
    #   area_output <- bind_rows(area_output, as.data.frame(matrix(NA, ncol = ncol(area_output), nrow = 1)))
    # }

    # add columns for LA name, county, region, country, and the iteration index
    # for the LA (useful for quickly identifying which LA the function reached
    # if it breaks unexpectedly)
    # area_output$la_name <- la_name
    # area_output$la_code <- la_code
    # area_output$county <- county
    # area_output$region <- region
    # area_output$country <- country
    # area_output$index <- h
    # area_output$number_months_acquired <- number_months_acquired
    # area_output$proportion_months_acquired <- number_months_acquired / num_months_backwards
    # area_output$force <- force
    #
    # # separate datetime into 2 columns
    # area_output$time <- substr(area_output$datetime, 12, 19)
    # area_output$date <- as.Date(substr(area_output$datetime, 1, 10))
    # # set as time
    # #area_output$time <- chron(times. =  df$time, format = c(times = "hh:mm:ss"))
    # save(area_output, file = "test.Rdata")


    # add records from this area (h) to overall output
    overall_output <- dplyr::bind_rows(overall_output, area_output)

    # create a temporary output list and save it every time a LA completes, so
    # that there is a backup in case function breaks. Saves to same folder as
    # script
    # save_progress <- list(result = overall_output,
    #                       missing_entries = no_entries_df,
    #                       server_errors = server_error_df,
    #                       last_area_acquired = h)
    # save(save_progress, file = "./save_progress.Rdata")
  }


  #### End: Area loop (h) ####

  # Add in area lookup
  # First just get msoas and above from lookup
  areas_above <- area_lookup %>%
    dplyr::select(c(lsoa21cd:rgn22nm)) %>%
    dplyr::distinct()

  # Then join
  overall_output <- overall_output %>%
    dplyr::left_join(areas_above, by = c("lsoa21cd", "lsoa21nm")) %>%
    # Locate the area variables at the left of the dataframe
    dplyr::relocate(c(msoa21cd:rgn22nm), .after = lsoa21nm)

  # If datetime exists in overall_output (and therefore stops were found),
  # relocate it. Otherwise don't try to relocate datetime (as it isn't there!)
  if("datetime" %in% colnames(overall_output)){
    overall_output <- overall_output %>%
      dplyr::relocate(datetime, .after = rgn22nm)
  }



  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units = "secs")
  cat(paste0("\nDone in ",round(time_elapsed, 3), " seconds"))

  return(overall_output)

}

#' Get stop and search data for Output Areas
#'
#' Acquire stop and search data from police.uk API for Output Areas (OAs).
#'
#' @param subset A named list defining the areas for which data are required. Names
#' correspond to the area variable on which to subset. Values correspond to the desired
#' values of the area variable. See `area_variables()` for areas that can be used to subset.
#' If NULL will acquire for all areas. Note this will take a very long time. Consider chunking
#' a large number of desired areas into separate calls.
#' @param num_months_backwards The number of months backwards from the start
#' point for which to acquire data. Default is 12. Maximum is 36 (limit imposed by API).
#' @param oldest_month Instead of specifying `num_months_backwards`, the user
#' can specify the oldest month of interest (in combination with the oldest year
#' of interest). Numeric value in format M (e.g., 8 for August).
#' @param oldest_year Instead of specifying `num_months_backwards`, the user
#' can specify the oldest year of interest (in combination with the oldest
#' month of interest). Numeric value in format YYYY (e.g., 2019).
#' @param most_recent_month The most recent month of interest. Numeric value in format M
#' (e.g., 8 for August). By default the function will determine this based on
#' the most recent API update.
#' @param most_recent_year The most recent month of interest. Numeric value in format YYYY
#' (e.g., 2021). By default the function will determine this based on the most
#' recent API update.
#' @param wait_time If there is a server error when submitting POST request,
#' the function will wait `wait_time` seconds before retrying. Numeric value.
#' Default is 5.
#' @param max_tries Specify the maximum number of times to retry a failed
#' POST request. Numeric value. Default is 10. Failures are usually due to
#' timeouts, which are often resolved by retrying, but can also be because there
#' is no data avaible for the area/date combination.
#' @param include_no_stop_areas Whether to include areas for which there are no results.
#' Default is TRUE (recommended as this will flag areas for which there are no stops and so
#' provide most comprehensive picture).
#'
#' @returns A data frame where each row is a separate stop and search record.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get the most recent 12 months of data for all OAs. This will take a long time!
#' # Don't run as will take a long time
#' df <- get_oa_data()
#'
#' # Get data for a subset of OAs, only the last 6 months
#' df2 <- get_oa_data(subset = list(
#'                       "lad22nm" = c('Haringey','Waltham Forest')),
#'                       num_months_backwards = 6)
#' }
#'
#'
get_oa_data <- function(subset = NULL,
                          num_months_backwards = 12,
                          oldest_month = NULL,
                          oldest_year = NULL,
                          most_recent_month = NULL,
                          most_recent_year = NULL,
                          wait_time = 5,
                          max_tries = 5,
                          include_no_stop_areas = TRUE,
                          cache = TRUE
                          # n_records = NULL # For testing
){

  policedatR::caching_check(cache)
  caching <- Sys.getenv("caching")
  cache_dir <- Sys.getenv("cache_dir")

  #### Error handling ####

  # Make sure that user can't specify an incorrect time period
  # 1. Too many months backwards
  if(!is.null(num_months_backwards)){
    if(num_months_backwards > 36){
      stop("Error in num_months_backwards. The API only holds data for the previous 36 months. Respecify num_months_backwards")
    }
  }

  # 2. Back to year/month specification exceeds 36 months backwards
  oldest_date <- as.Date(paste0(oldest_data(),"-01"))
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    if(as.Date(paste0(oldest_year,"-",oldest_month,"-01")) < oldest_date){
      stop("Error in oldest_month/oldest_year. The API only holds data for the previous 36 months. The oldest date is ", substr(oldest_date, 1, 7))
    }
  }

  # 3. Most recent specification is too recent
  newest_date <- as.Date(paste0(newest_data(),"-01"))
  if(!is.null(most_recent_month) && !is.null(most_recent_year)){
    if(as.Date(paste0(most_recent_year,"-", most_recent_month,"-01")) > newest_date){
      stop("Error in most_recent_month/most_recent_year. The most recent data available is ", substr(newest_date, 1, 7))
    }
  }

  # 4. Date not fully specified
  ## one of the 'oldest' arguments missing
  if((!is.null(oldest_month) && is.null(oldest_year)) ||
     (is.null(oldest_month) && !is.null(oldest_year))){
    stop("Error: Both oldest_month and oldest_year must be specified.")
  }
  ## one of the 'most_recent' arguments missing
  if((!is.null(most_recent_month) && is.null(most_recent_year)) ||
     (is.null(most_recent_month) && !is.null(most_recent_year))){
    stop("Error: Both most_recent_month and most_recent_year must be specified.")
  }
  ## most recent specified with no target in the past (with default setting for num_months_backwards this won't happen)
  if(!is.null(most_recent_month) && !is.null(most_recent_year) &&
     is.null(num_months_backwards) &&
     (is.null(oldest_month) || is.null(oldest_year))){
    stop("Error: Time period not specified or incomplete. Either num_months_backwards or both oldest_month and oldest_year must be specified")
  }



  t1 <- Sys.time()

  #### Get geometries ####

  geometries <- policedatR::get_oa_geometries(subset)


  # Initialise variables
  overall_output <- data.frame()
  # no_entries_df <- data.frame(setNames(rep(list(NA), 6), c("Index", "Name","County","Region","Country","Force")))
  # server_error_df <- data.frame(setNames(rep(list(NA), 10), c("Index","Name","County","Region","Country","Force","Date", "Date_Index", "Coordinate_Set_Index", "Status_Code")))

  # get most recent update if to data not specified
  if(is.null(most_recent_month) || is.null(most_recent_year)){
    # get most recent update from API:
    date <- httr::content(
      httr::GET("https://data.police.uk/api/crimes-street-dates"))[[1]][["date"]]
    most_recent_month <- as.numeric(substr(date,6,7))
    most_recent_year <- as.numeric(substr(date,1,4))
  }
  else{
    most_recent_month <- most_recent_month
    most_recent_year <- most_recent_year
  }

  # if oldest_month/year has been specified, redefine num_months_backwards
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    num_months_backwards <- 1 + (12 * (most_recent_year - oldest_year)) + (most_recent_month - oldest_month)
  }
  else{
    num_months_backwards <- num_months_backwards
  }

  # For testing - subset to smaller number of geometries
  # if(!is.null(n_records)){
  #   # only adjust geometries if n_records is smaller than nrow(geometries)
  #   if(n_records < nrow(geometries)){
  #     geometries <- geometries[1:n_records,]
  #   }
  # }



  #### h loop: iterates over areas ####

  for(h in 1:nrow(geometries)){
    cat(paste0("\nStarted area ", h)) # report start (useful for debugging)

    area_output <- data.frame() # initialise area output df
    number_months_acquired <- 0 # initialise number of months acquired counter

    #### i loop: iterates over the months required ####

    for(i in 1:num_months_backwards){
      month_output <- data.frame()

      # format date to what is needed for API query ("yyyy-mm")
      if(i == 1){ # set values for first iteration
        month_num <- most_recent_month
        year <- most_recent_year
      }
      else{ # subsequent iterations
        month_num <- month_num - 1 # backwards a month each iteration
        if(month_num %% 12 == 0){ # if reach a new year, start months from 12 again
          month_num <- 12
          year <- year - 1 # backwards a year
        }
      }
      if(month_num < 10){ # paste 0 for months lower than 10
        month <- paste("0", month_num, sep = "")
      }
      else{
        month <- month_num
      }

      date <- paste(year, "-", month, sep = "") # combine dates into one string

      #### j loop ####
      # Iterates over the coordinate sets within each LA and creates a
      # polygon string to be searched, and then submits the query.
      # Most LAs have only one coordinate set, but some have multiple (e.g.,
      # those that include islands). The function therefore needs to search
      # each coordinate set within a LA separately.

      # Identify the column number for geometry
      geometry_column_number <- which(colnames(geometries) == "geometry")

      # Initialise vector for coord string
      coord_string <- c()

      for(j in 1:length(geometries[[geometry_column_number]][[h]])){

        # set this iteration's coordinate set
        # We must differentiate between polygons and multipolygons. When choosing
        # subsets it is possible the result will be polygon (where the area is just
        # polygon rather than multiple). If this is the case geometry is one list
        # less nested than the multipolygon case
        area_coords <- if(
          # if the hth, jth element of the geometry column is a list, go a level deeper
          # otherwise just pick the hth, jth elemen
          is.list(geometries[[geometry_column_number]][[h]][[j]])){
          geometries[[geometry_column_number]][[h]][[j]][[1]]
        } else {
          geometries[[geometry_column_number]][[h]][[j]]
        }

        area_coords <- area_coords %>%
          tibble::as_tibble(.name_repair = ~c("long","lat"))
        # long is e.g. -1.13
        # lat is e.g. 52.62

        # Create an areas df with no geometry column as placeholder in case no
        # data found for this area. This is so we can still count areas where
        # no data was found. It's also to provide area info for lower geography
        # cases (not relevant for LADs)
        areas_df <- geometries[h,] %>%
          sf::st_drop_geometry()

        # combine coord strings into format required by API (much quicker than looping)
        coord_string <- paste0(area_coords$lat,",",area_coords$long, collapse = ":")

        # create body for post request
        body <- list("poly" = coord_string,
                     "date" = date)

        if(caching){
          cd = cachem::cache_disk(cache_dir, evict = "lru")
          # Memoise the function
          fetch_police_data_memoised <- memoise::memoise(fetch_police_data, cache = cd)
          post_request <- fetch_police_data_memoised(body = body)
        }
        else {
          post_request <- fetch_police_data(body = body)
        }


        # get data from results of query
        df <- httr::content(post_request)

        # unlist data and convert to dataframe
        df_2 <- lapply(df, unlist)
        df_3 <- do.call(dplyr::bind_rows, df_2)
        df_3$coord_set <- j # record which coordinate set data is from

        # add results of this coordinate set iteration (j) to the output for
        # this month iteration (i). Use bindrows for (high) possibility that columns
        # from different iterations will be in different order/missing
        month_output <- dplyr::bind_rows(month_output, df_3)

        cat("\014") # clear console
        # report overall (i.e., LA) progress
        cat(paste0("Working... Area ", h, " of ", nrow(geometries),
                   " (",
                   round(100 * (h / nrow(geometries)), 2), "%)"))
        # report month progress
        cat(paste0("\nWorking... Month ", i, " of ", num_months_backwards,
                   " (", date, ")"))
        # report coordinate set progress
        cat(paste0("\nWorking... ", j, " of ",
                   length(geometries[[geometry_column_number]][[h]]),
                   " coordinate sets retrieved"))

      }

      #### End: Coordinate set loop (j) ####



      # If records have been acquired, increase months count
      if(nrow(month_output) > 0){
        number_months_acquired <- number_months_acquired + 1
      }

      # Add data from this month (i) to overall area output (h)
      area_output <- dplyr::bind_rows(area_output, month_output)

    }

    #### End: Month loop (i) ####

    # If there were no records for this LA, record the LA iteration number
    # and name, then proceed to the next LA
    # if(nrow(area_output) == 0){
    #   if(is.na(no_entries_df[1,1])){ # if first occurrence, replaces NAs
    #     no_entries_df[1,] <- c(h, la_name, county, region, country, force)
    #   }
    #   else{ # rbind subsequent occurrences
    #     no_entries_df <- rbind(no_entries_df, c(h, la_name, county, region, country, force))
    #   }
    #   print(paste0("No records for ", la_name))
    #   next # proceed to next LA
    # }
    # print(nrow(area_output))
    # print(area_output)

    # If the area output is empty, no stops were found in the area.
    # where this is the case, report the area data, with NAs for the rest
    # and explicitly tag these areas as having no stops with stops_found = FALSE
    if(include_no_stop_areas==TRUE){
      if(nrow(area_output) == 0){
        area_output <- areas_df %>% # add in all the area data by binding the columns for this area
          dplyr::mutate(
            stops_found = FALSE
          )
      } else{
        # if stops were found for this area, add in the areas to the front
        # and tag the rows with stops_found = TRUE
        area_output <- dplyr::bind_cols(areas_df, area_output) %>%
          dplyr::mutate(
            stops_found = TRUE
          )
      }
    }


    # if(nrow(area_output==0)){
    #   area_output <- bind_rows(area_output, as.data.frame(matrix(NA, ncol = ncol(area_output), nrow = 1)))
    # }

    # add columns for LA name, county, region, country, and the iteration index
    # for the LA (useful for quickly identifying which LA the function reached
    # if it breaks unexpectedly)
    # area_output$la_name <- la_name
    # area_output$la_code <- la_code
    # area_output$county <- county
    # area_output$region <- region
    # area_output$country <- country
    # area_output$index <- h
    # area_output$number_months_acquired <- number_months_acquired
    # area_output$proportion_months_acquired <- number_months_acquired / num_months_backwards
    # area_output$force <- force
    #
    # # separate datetime into 2 columns
    # area_output$time <- substr(area_output$datetime, 12, 19)
    # area_output$date <- as.Date(substr(area_output$datetime, 1, 10))
    # # set as time
    # #area_output$time <- chron(times. =  df$time, format = c(times = "hh:mm:ss"))
    # save(area_output, file = "test.Rdata")


    # add records from this area (h) to overall output
    overall_output <- dplyr::bind_rows(overall_output, area_output)

    # create a temporary output list and save it every time a LA completes, so
    # that there is a backup in case function breaks. Saves to same folder as
    # script
    # save_progress <- list(result = overall_output,
    #                       missing_entries = no_entries_df,
    #                       server_errors = server_error_df,
    #                       last_area_acquired = h)
    # save(save_progress, file = "./save_progress.Rdata")
  }


  #### End: Area loop (h) ####

  # Add in area lookup
  # First just get msoas and above from lookup
  areas_above <- area_lookup %>%
    dplyr::select(c(oa21cd:rgn22nm)) %>%
    dplyr::distinct()

  # Then join
  overall_output <- overall_output %>%
    dplyr::left_join(areas_above, by = c("oa21cd")) %>%
    # Locate the area variables at the left of the dataframe
    dplyr::relocate(c(lsoa21cd:rgn22nm), .after = oa21cd)

  # If datetime exists in overall_output (and therefore stops were found),
  # relocate it. Otherwise don't try to relocate datetime (as it isn't there!)
  if("datetime" %in% colnames(overall_output)){
    overall_output <- overall_output %>%
      dplyr::relocate(datetime, .after = rgn22nm)
  }

  t2 <- Sys.time()
  time_elapsed <- difftime(t2, t1, units = "secs")
  cat(paste0("\nDone in ",round(time_elapsed, 3), " seconds"))

  return(overall_output)

}

