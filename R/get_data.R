get_oa_data <- function(area = NULL,
                        num_months_backwards = 12,
                        oldest_month = NULL,
                        oldest_year = NULL,
                        most_recent_month = NULL,
                        most_recent_year = NULL,
                        wait_time = 5,
                        max_tries = 5,
                        include_no_stop_areas = TRUE){

  ########################
  #### Error handling ####
  ########################

  # If user doesn't specify a subset area, check they're sure they want to
  # download data for all output areas
  if(is.null(area)){
    check <- readline("Acquiring data for all Output Areas will
                      take approximately 20 minutes. You can specify an area to
                      restrict you query using 'area'. Do you want to continue
                      acquiring data for all Output Areas? (y/n)")
    if(check == "n"){
      break
    } else{
      print("Acquiring data for all Output Areas")
    }

  }

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

  ###########################
  ### End: Error handling ###
  ###########################



}


get_lad_data <- function(subset = NULL,
                         num_months_backwards = 12,
                         oldest_month = NULL,
                         oldest_year = NULL,
                         most_recent_month = NULL,
                         most_recent_year = NULL,
                         wait_time = 5,
                         max_tries = 5,
                         include_no_stop_areas = TRUE,
                         n_records = 3 # For testing
                         ){

  ########################
  #### Error handling ####
  ########################

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

  ###########################
  ### End: Error handling ###
  ###########################

  ########################
  #### Get geometries ####
  ########################

  geometries <- policedatR::get_lad_geometries(subset)

  #############################
  #### End: Get geometries ####
  #############################

  # Initialise variables
  overall_output <- data.frame()
  no_entries_df <- data.frame(setNames(rep(list(NA), 6), c("Index", "Name","County","Region","Country","Force")))
  server_error_df <- data.frame(setNames(rep(list(NA), 10), c("Index","Name","County","Region","Country","Force","Date", "Date_Index", "Coordinate_Set_Index", "Status_Code")))

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
  if(!is.null(n_records)){
    # only adjust geometries if it n_records is smaller than nrow(geometries)
    if(n_records < nrow(geometries)){
      geometries <- geometries[1:n_records,]
    }
  }

  #####################################
  #### h loop: iterates over areas ####
  #####################################

  for(h in 1:nrow(geometries)){
    cat(paste0("\nStarted area ", h)) # report start (useful for debugging)

    area_output <- data.frame() # initialise area output df
    number_months_acquired <- 0 # initialise number of months acquired counter

    ###################################################
    #### i loop: iterates over the months required ####
    ###################################################

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

      ################
      #### j loop ####
      ################
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
            } %>%
          tibble::as_tibble() %>% # .name_repair = ~c("long","lat")) # %>%
          dplyr::rename(
            lat = V2, # lat is e.g. 52.62
            long = V1 # long is e.g. -1.13
          )

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

        # search API for this coordinate set and date:
        post_request <- httr::POST("https://data.police.uk/api/stops-street?", body = body)

        # if search quota reached, break (shouldn't be an issue but just in case)
        if(post_request[["status_code"]] == 429){
          print("Quota reached. Abandoning request.")
          break
        }
        else{
          # if the request didn't succeed, wait some time ('wait_time') and
          # keep trying up until 'max_tries' attempts.
          attempt <- 1
          while(post_request[["status_code"]] != 200 && attempt <= max_tries){
            print(paste0("Server error: ", post_request[["status_code"]], ". Trying again (", attempt,")"))
            Sys.sleep(wait_time) # wait some time before trying again
            try(
              post_request <- httr::POST("https://data.police.uk/api/stops-street?", body = body)
            )
            # if search quota reached, break (shouldn't be an issue but just in case)
            if(post_request[["status_code"]] == 429){
              print("Quota reached. Abandoning request.")
              break
            }
            attempt <- attempt + 1
          }

          # once max_tries is met, give up retry, save info including status code
          if(post_request[["status_code"]] != 200 && attempt > max_tries){
            print(paste0("Max tries reached (", max_tries,"). Continuing."))
            if(is.na(server_error_df[1,1])){ # if first occurrence, replaces NAs
              server_error_df[1,] <- c(h, la_name, county, region, country, force, date, i, j, post_request[["status_code"]])
            }
            else{ # rbind subsequent occurrences
              server_error_df <- rbind(server_error_df, c(h, la_name, county, region, country, force, date, i, j, post_request[["status_code"]]))
            }
          }
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

      #####################################
      #### End: Coordinate set loop (j) ####
      #####################################



      # If records have been acquired, increase months count
      if(nrow(month_output) > 0){
        number_months_acquired <- number_months_acquired + 1
      }

      # Add data from this month (i) to overall area output (h)
      area_output <- dplyr::bind_rows(area_output, month_output)

    }

    #############################
    #### End: Month loop (i) ####
    #############################

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

    # this is an optional bit to explcitly flag areas where data are missing. i did this originally to double check that the sciprt was definitely doing a post request for every oa. but it seems good practice to inclue this as least as an option, so we know for which areas there were no stops
    if(include_no_stop_areas==TRUE){
      # if the area output is empty, no stops were found in the area.
      # where this is the case, report the area data, with NAs for the rest
      if(nrow(area_output) == 0){
        # cat("nrow is 0")
        area_output <- areas_df # add in all the area data by binding the columns for this area
      } else{
        # cat("nrow not 0")
        # if stops were found for this area, add in the areas to the front
        area_output <- bind_cols(areas_df, area_output)
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

  ############################
  #### End: Area loop (h) ####
  ############################
  return(overall_output)

}

