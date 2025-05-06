# v2 has mixed black categories factored into black



analyse_records <- function(data,

                            ethnicity_definition = c("self","officer"),
                            collapse_ethnicity = TRUE,
                            comparison = c("white","black"),
                            date = c("by_year", "by_month","12_month_periods")){
  # load stop records
  # check if data file is an object in environ or a csv
  # data <- if(is.object(data_file)) data_file else read.csv(data_file)
  # # subset to England & Wales
  # data <- data %>%
  #   subset(., country != "Northern Ireland" & country != "Scotland")
  #
  # # add region for Wales
  # data$region[data$country == "Wales"] <- "Wales"

  # Force collapse ethnicity if ethnicity definition is 'officer' becuase
  # officer-defined ethnicity is only the 5 category ethnicity
  if(ethnicity_definition == "officer"){
    collapse_ethnicity <- TRUE
  }

  create_ethnicity_names <- function(population_ests, ethnicity_definition){
      # Get just the aggregated names - "Census 2021 Ethnic group classification 6a"
      ethnicity_names <- population_ests %>%
        dplyr::distinct(ethnicity) %>%
        dplyr::pull()

      # Order the ethnicities alphabetically
      ethnicity_names <- ethnicity_names[order(ethnicity_names)]
      if(ethnicity_definition == "self"){
        # Add not stated category as this is not found in population estimates
        # Only for self as 'not stated' doesn't exist for officer-defined
        ethnicity_names <- c(ethnicity_names, "not_stated")
      }

    return(ethnicity_names)
  }


  population_ests <- policedatR::get_population_estimates(data, collapse_ethnicity)

  # # Tidy up the ethnicity names in population estimates for refactoring
  ethnicity_names <- create_ethnicity_names(population_ests, ethnicity_definition)

  browser()

  if(ethnicity_definition == "self"){
    # browser()
    if(collapse_ethnicity==TRUE){
      # Map out the ethnicities in police data into aggregated bins
      ethnicity_map <- list(
        c(
          "Asian/Asian British - Any other Asian background",
          "Asian/Asian British - Bangladeshi",
          "Asian/Asian British - Chinese",
          "Asian/Asian British - Indian",
          "Asian/Asian British - Pakistani"),
        c(
          "Black/African/Caribbean/Black British - African",
          "Black/African/Caribbean/Black British - Any other Black/African/Caribbean background",
          "Black/African/Caribbean/Black British - Caribbean"),
        c(
          "Mixed/Multiple ethnic groups - Any other Mixed/Multiple ethnic background",
          "Mixed/Multiple ethnic groups - White and Asian",
          "Mixed/Multiple ethnic groups - White and Black African", # have included mixed in Black category
          "Mixed/Multiple ethnic groups - White and Black Caribbean"),
        c(
          "Other ethnic group - Any other ethnic group",
          "Other ethnic group - Arab"),
        c(
          "White - Any other White background",
          "White - English/Welsh/Scottish/Northern Irish/British",
          "White - Irish",
          "White - Gypsy or Irish Traveller"),
        c("Other ethnic group - Not stated")
      )


    }  else{
      # Map out the ethnicities in police data into aggregated bins
      # Order looks strange but it's matching the alphabetically ordered list
      # of clean names from population estimates
      ethnicity_map <- list(
        "Black/African/Caribbean/Black British - African",
        "Other ethnic group - Any other ethnic group",
        "Other ethnic group - Arab",
        "Asian/Asian British - Bangladeshi",
        "Black/African/Caribbean/Black British - Caribbean",
        "Asian/Asian British - Chinese",
        "White - English/Welsh/Scottish/Northern Irish/British",
        "White - Gypsy or Irish Traveller",
        "Asian/Asian British - Indian",
        "White - Irish",
        "Asian/Asian British - Any other Asian background",
        "Black/African/Caribbean/Black British - Any other Black/African/Caribbean background",
        "Mixed/Multiple ethnic groups - Any other Mixed/Multiple ethnic background",
        "White - Any other White background",
        "Asian/Asian British - Pakistani",
        "Mixed/Multiple ethnic groups - White and Asian",
        "Mixed/Multiple ethnic groups - White and Black African",
        "Mixed/Multiple ethnic groups - White and Black Caribbean",
        "Other ethnic group - Not stated"
        )
    }
    #
    # # Apply aggregated ethnicity names to the bins
    ethnicity_map <- setNames(ethnicity_map, ethnicity_names)
    #
    # Recode self-defined ethnicity in police data using unquote-splice operator !!!
    data$self_defined_ethnicity <- forcats::fct_collapse(
      data$self_defined_ethnicity,
      !!!ethnicity_map
    )

    data <- data %>%
      dplyr::mutate(
        ethnicity = self_defined_ethnicity
      )
  }
  else if(ethnicity_definition == "officer"){
    data <- data %>%
      dplyr::mutate(
        ethnicity = stringr::str_to_lower(officer_defined_ethnicity)
      )
  }

  browser()

  area_variable <- colnames(data)[1] # Can add in the other area variables after
  summarised_data <- data %>%
    dplyr::group_by(!!rlang::sym(area_variable), ethnicity) %>%
    dplyr::summarise(
      stopped = dplyr::n()
    ) %>%
    dplyr::full_join(population_ests, by = c(area_variable, "ethnicity"))

  browser()


  ethnicity_1 <- comparison[1]
  ethnicity_2 <- comparison[2]

  # subset to ethnicities to compare
  data_subset <- subset(data, ethnicity == ethnicity_1 | ethnicity == ethnicity_2)
  # refactor. First in comparison is reference, second is treatment
  data_subset$ethnicity <- factor(data_subset$ethnicity, levels = comparison)

  # separate date into separate columns
  data_subset$year <- as.numeric(substr(data_subset$datetime, 1, 4))
  data_subset$month <- as.numeric(substr(data_subset$datetime, 6, 7))
  data_subset$day <- as.numeric(substr(data_subset$datetime, 9, 10))

  # make year_month variable for indexing
  data_subset$year_month <-
    as.Date(paste(
      as.character(data_subset$year),
      as.character(data_subset$month),"01", sep= "-"))

  # if date by year, take sequence from min to max
  if(date == "by_year"){
    dates <- min(data_subset$year):max(data_subset$year)
  }
  # if date by month, take sequence of each month from start
  else if(date == "by_month"){
    date_set <- unique(data_subset$year_month)
    dates <- date_set[order(date_set)]
  }
  # if date by 12 months, define oldest year and month and create boundaries
  # for each 12 months
  else if(date == "12_month_periods"){
    oldest_year <- min(data_subset$year)
    oldest_month <- as.numeric(substr(min(data_subset$year_month), 6, 7))

    # does the 12 months cross to a new year?
    passes_year <- if(oldest_month + 11 > 12) TRUE else FALSE

    # calculate start month for next 12 months
    next_month <- if(oldest_month > 1) oldest_month - 1 else 12

    # create list to be indexed for defining 12 month boundary
    dates <- list(
      # first 12 months
      c(as.Date(paste(oldest_year, oldest_month, "01", sep = "-"), format = "%Y-%m-%d"),
        as.Date(paste(if(passes_year) oldest_year + 1 else oldest_year,
                      next_month, "01", sep = "-"),format = "%Y-%m-%d")),
      # second 12 months
      c(as.Date(paste(if(passes_year) oldest_year + 2 else oldest_year + 1,
                      oldest_month, "01", sep = "-"),  format = "%Y-%m-%d"),
        as.Date(paste(if(passes_year) oldest_year + 2 else oldest_year + 1,
                      next_month, "01", sep = "-"),  format = "%Y-%m-%d")),
      # third 12 months
      c(as.Date(paste(if(passes_year) oldest_year + 3 else oldest_year + 2,
                      oldest_month, "01", sep = "-"),  format = "%Y-%m-%d"),
        as.Date(paste(if(passes_year) oldest_year + 3 else oldest_year + 2,
                      next_month, "01", sep = "-"),  format = "%Y-%m-%d")))

  }

  # Summarisation starts here

  # The code below creates a contingency table for each LAD and runs chi-square
  # and Fisher's exact tests on it.
  # It also creates an overall contingency table on which to base combined statistics.

  # data_subset$la_name[which(data_subset$la_name == "Rhondda Cynon Taf")] <- "Rhondda Cynon Taff"
  # data_subset$region[which(data_subset$region == "Yorkshire and The Humber")] <- "Yorkshire and the Humber"



  # create dummy area variable
  data_subset[,"area"] <- data_subset[,2]
  areas <- unique(data_subset$area) %>% pull()
  # initialise
  all_results <- data.frame()
  all_mats <- matrix(data = c(0,0,0,0), ncol = 2, nrow = 2)
  all_dfs <- data.frame()
  count <- 0

  # regional fork would be here

  for(i in 1:length(areas)){
    results_df <- data.frame() # initialise
    # get data for this area
    this_area <- unique(data_subset[which(data_subset$area == areas[i]), 2]) %>% pull()
    # county <- unique(data_subset[which(data_subset$la_name == las[i]), "county"])
    # region <- unique(data_subset[which(data_subset$la_name == las[i]), "region"])
    # country <- unique(data_subset[which(data_subset$la_name == las[i]), "country"])
    # force <- unique(data_subset[which(data_subset$la_name == las[i]), "force"])

    for(j in 1:length(dates)){
      if(date == "by_year"){
        this_date <- dates[j]
        temp_df <- data_subset %>% # subset to la
          filter(area == this_area & year == this_date)
      }
      else if(date == "by_month"){
        this_date <- dates[j]
        temp_df <- data_subset %>% # subset to la
          subset(., area == this_area & year_month == this_date)
      }
      else if(date == "12_month_periods"){
        this_date <- dates[[j]]
        temp_df <- data_subset %>% # subset to la
          subset(., area == this_area & year_month >= this_date[1] & year_month <= this_date[2])
        # rename this date for variable name later
        this_date <- paste0("12 months to ", substr(dates[[j]][2], 1, 7))
      }

      # need also to consider istuatoin where both are missing
      if(length(unique(temp_df$ethnicity)) == 2){ # &
        # !is.na(population_ests[which(population_ests$LAD == la), ethnicity_1]) &
        # !is.na(population_ests[which(population_ests$LAD == la), ethnicity_2])){ # check that there are stops for both white and black individuals

        # collect stats
        temp_df <- temp_df %>%
          dplyr::group_by(ethnicity) %>%
          dplyr::summarise(
            stopped = dplyr::n()
          ) %>%
          dplyr::mutate(
            pop = c(as.numeric(population_ests[which(population_ests$area == this_area), ethnicity_1]),
                    as.numeric(population_ests[which(population_ests$area == this_area), ethnicity_2])),
            percentage = 100 * (stopped/pop),
            not_stopped = pop - stopped
          ) %>%
          as.data.frame()
      } else if(length(unique(temp_df$ethnicity)) == 1){
        # get the missing ethnicity if one is missing - why would tehre be a missing ehtnicity again? ah becuase tehre are no counts for it
        missing_ethnicity <- comparison[which(!(comparison %in% temp_df$ethnicity))]
        data_to_insert <- data.frame("ethnicity" = missing_ethnicity, "stopped" = 0)

        temp_df <- temp_df %>%
          dplyr::group_by(ethnicity) %>%
          dplyr::summarise(
            stopped = dplyr::n()
          )

        temp_df <- bind_rows(temp_df, data_to_insert) %>%
          dplyr::mutate(
            pop = c(as.numeric(population_ests[which(population_ests$area == this_area), ethnicity_1]),
                    as.numeric(population_ests[which(population_ests$area == this_area), ethnicity_2])),
            percentage = 100 * (stopped/pop),
            not_stopped = pop - stopped
          ) %>%
          as.data.frame()
      }
      # note here we need to also account for no ehtnicities

      #anotehr condition to run stats only if both ethnictiues are presnet
      # this is the OR aprt

      if(length(unique(temp_df$ethnicity)) == 2){ # &

        row.names(temp_df) <- comparison

        ethn_1 <- data.frame("ethnicity_1" = c("stopped" = temp_df[ethnicity_1, "stopped"],
                                               "not_stopped" = temp_df[ethnicity_1, "not_stopped"]))

        ethn_2 <- data.frame("ethnicity_2" = c("stopped" = temp_df[ethnicity_2, "stopped"],
                                               "not_stopped" = temp_df[ethnicity_2, "not_stopped"]))

        comp_mat <- as.matrix(cbind(ethn_2, ethn_1)) # matrix for crosstable
        comp_df <- as.data.frame(comp_mat) # df for custom rr function

        if(sum(is.na(comp_mat)) == 0){ # if there are figures for all cells, run stats
          # use tryCatch to collect warning messages
          xtab <- tryCatch(gmodels::CrossTable(comp_mat, chisq = T, fisher = T, expected = T),
                           warning = function(w) return(list(gmodels::CrossTable(comp_mat, chisq = T, fisher = T, expected = T), w)))
          rr <- riskratio_from_df(comp_df, "Stop & Search")
          results_df <- data.frame(
            "date" = this_date,
            "area" = this_area,
            # "county" = county,
            # "region" = region,
            # "country" = country,
            # "force" = force,
            "ethnicity_2_stopped" = temp_df[ethnicity_2, "stopped"],
            "ethnicity_2_not_stopped" = temp_df[ethnicity_2, "not_stopped"],
            "ethnicity_2_population" = temp_df[ethnicity_2, "pop"],
            "ethnicity_2_stop_rate" = temp_df[ethnicity_2, "percentage"],
            "ethnicity_1_stopped" = temp_df[ethnicity_1, "stopped"],
            "ethnicity_1_not_stopped" = temp_df[ethnicity_1, "not_stopped"],
            "ethnicity_1_population" = temp_df[ethnicity_1, "pop"],
            "ethnicity_1_stop_rate" = temp_df[ethnicity_1, "percentage"],
            "or" = ifelse(is.list(xtab[[1]]),
                          xtab[[1]][["fisher.ts"]][["estimate"]][["odds ratio"]],
                          xtab[["fisher.ts"]][["estimate"]][["odds ratio"]]),
            "or_ci_low" = ifelse(is.list(xtab[[1]]),
                                 xtab[[1]][["fisher.ts"]][["conf.int"]][1],
                                 xtab[["fisher.ts"]][["conf.int"]][1]),
            "or_ci_upp" = ifelse(is.list(xtab[[1]]),
                                 xtab[[1]][["fisher.ts"]][["conf.int"]][2],
                                 xtab[["fisher.ts"]][["conf.int"]][2]),
            "rr" = rr$rr,
            "rr_ci_low" = rr$ci_low,
            "rr_ci_upp" = rr$ci_upp,
            "warning" = ifelse(is.list(xtab[[1]]), xtab[[2]][["message"]], NA))

          all_mats <- all_mats + comp_mat
          count <- count + 1 # increase count of areas for which stats have been acquired

        }
        else { # if there are missing values, don't run stats but still add frequency data
          results_df <- data.frame(
            "date" = this_date,
            "area" = this_area,
            # "county" = county,
            # "region" = region,
            # "country" = country,
            # "force" = force,
            "ethnicity_2_stopped" = temp_df[ethnicity_2, "stopped"],
            "ethnicity_2_not_stopped" = temp_df[ethnicity_2, "not_stopped"],
            "ethnicity_2_population" = temp_df[ethnicity_2, "pop"],
            "ethnicity_2_stop_rate" = temp_df[ethnicity_2, "percentage"],
            "ethnicity_1_stopped" = temp_df[ethnicity_1, "stopped"],
            "ethnicity_1_not_stopped" = temp_df[ethnicity_1, "not_stopped"],
            "ethnicity_1_population" = temp_df[ethnicity_1, "pop"],
            "ethnicity_1_stop_rate" = temp_df[ethnicity_1, "percentage"],
            "or" = NA,
            "or_ci_low" = NA,
            "or_ci_upp" = NA,
            "rr" = NA,
            "rr_ci_low" = NA,
            "rr_ci_upp" = NA,
            "warning" = NA)
        }
      }
        else { # if there is not data for both black and white, and/or there are missing values
        results_df <- data.frame(
          "date" = this_date,
          "area" = this_area,
          # "county" = county,
          # "region" = region,
          # "country" = country,
          # "force" = force,
          "ethnicity_2_stopped" = NA,
          "ethnicity_2_not_stopped" = NA,
          "ethnicity_2_population" = NA,
          "ethnicity_2_stop_rate" = NA,
          "ethnicity_1_stopped" = NA,
          "ethnicity_1_not_stopped" = NA,
          "ethnicity_1_population" = NA,
          "ethnicity_1_stop_rate" = NA,
          "or" = NA,
          "or_ci_low" = NA,
          "or_ci_upp" = NA,
          "rr" = NA,
          "rr_ci_low" = NA,
          "rr_ci_upp" = NA,
          "warning" = NA)
      }


      all_results <- rbind(all_results, results_df) # add results to all results
      all_dfs <- as.data.frame(all_mats)
      cat("\014")
      print(paste0(i, " of ", length(areas), " complete (", round(100 * (i / length(areas)),2 ), "%)"))
    }

  }

  # add names to ethnicity categories
  names(all_results) <- sub("^ethnicity_1", ethnicity_1, names(all_results))
  names(all_results) <- sub("^ethnicity_2", ethnicity_2, names(all_results))

  return(all_results)

}
