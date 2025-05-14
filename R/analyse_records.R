#' Count stops
#'
#' Count the number of stops for combinations of areas, time periods, and ethnicities.
#' If `comparison` is NULL, returns a long tibble where each row is a unique
#' area-period-ethnicity combination across all ethnicities. Computed statistics are
#' count and rate per 1000 population. If `comparison` is not NULL, performs a
#' generalised linear regression using Poisson distribution and log link function
#' to produce an incidence rate ratio describing the extent of the different between
#' the stop rates of the two ethnicities specified. In this case, it returns a wide tibble
#' where each row is a unique area-period combination across only the ethnicities
#' specified. Computed statistics are count, rate per 1000 population, incidence rate ratio,
#' lower and upper confidence interval values, and p-value.
#'
#' @param data A data frame extracted using policedatR where each row is a unique
#' stop and the first column is the area code variable for which stops were acquired
#' (e.g. lad22cd for Local Authority Districts).
#' @param ethnicity_definition String specifying which ethnicity definition to use
#' for counts. 'self' has the possibility of using the 18 disaggregated categories but is likely
#' to have more NAs than 'officer'. 'officer' is only the 5 aggregated ethnicity categories and
#' will likely have fewer NAs (technically it should have 0 but in practice this isn't the case)
#' @param collapse_ethnicity If `ethnicity_definition == 'self'`, this controls
#' whether to use the 18 disaggregated categories or to aggregate to the 5 broader
#' categories. If `ethnicity_definition == 'self'`, `collapse_ethnicity` is always TRUE.
#' @param comparison User can optionally choose to compare between two ethnicities. This is
#' a character vector with length 2, where the first element is the baseline against which the
#' second element is compared. If this is specified, counts, rates, and incidence rate ratios will
#' be returned for only the two ethnicities specified.
#' @param period Numeric value specifying the number of months each time period
#' should be.
#'
#' @returns If `comparison` is NULL, a long tibble where each row is a unique
#' area-period-ethnicity combination across all ethnicities. Computed statistics are
#' count and rate per 1000 population.
#'
#' If `comparison` is specified, a wide tibble where each row is a unique
#' area-period combination across only the ethnicities specified. Computed statistics are
#' count, rate per 1000 population, incidence rate ratio, lower and upper confidence
#' interval, and p-value.
#'
#' @export
#'
#' @examples
count_stops <- function(data,
                            ethnicity_definition = c("self","officer"),
                            collapse_ethnicity = TRUE,
                            comparison = NULL,
                            period = 12){

  #### Helper functions ####

  # Specify a function that checks the input period is a factor of the total
  # unique months in data and warns the user if not. Suggests factors for the
  # total number of months, but user can still continue with desired period.
  # Also checks that user does not enter a value for period greater than the
  # total number of months in data.

  # The function will return the revised value of period, or if period is already
  # a factor, it will return the same value back.
  period_is_factor <- function(period, length_dates){
    if(period > length_dates){
      stop(paste0("Period cannot be longer than the total number of months in data (",length_dates,"). Please choose a period less than or equal to the total number of months in data."))
    }
    else{
      if(length_dates %% period != 0){
        factors <- c()
        for(i in 1:length_dates){
          if(length_dates %% i == 0){
            factors <- append(factors, i)
          }
        }

        cat("\nWarning: Requested period is not a factor of the total number of months in data. Your last period will have fewer months.")
        cat("\nThese periods are factors and might be better: ")
        cat(factors)
        repeat{
          check <- readline("Enter the period you would like to continue with: ")
          check <- as.numeric(check)
          if(check > length_dates){
            cat("\nPlease choose a period less than or equal to the total number of months in data.")
            cat("\nThese periods are factors and would be suitable: ")
            cat(factors)
          }
          else{
            cat(paste0("Continuing with ", check, "-month periods."))
            break
          }
        }
      }
      else{
        if(period == length_dates){
          cat(paste0("\nAnalysing across all months in data (",period,") as one."))
        }
        check <- period
      }

    }

    return(check)
  }


  # This function takes the ethnicity levels from population estimates which
  # can then be used to remap the ethnicity levels in the police data. This
  # facilitates joining the two in the analysis step.
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

  #### Dates ####

  # Prepare the dates for the period check
  # separate date into separate columns
  data$year <- as.numeric(substr(data$datetime, 1, 4))
  data$month <- as.numeric(substr(data$datetime, 6, 7))
  data$day <- as.numeric(substr(data$datetime, 9, 10))

  # make year_month variable for indexing - na problems here?
  data <- data %>%
    dplyr::mutate(
      year_month = dplyr::if_else(!is.na(year),
                          as.Date(paste(
                            as.character(data$year),
                            as.character(data$month),"01", sep= "-"), format = "%Y-%m-%d"), NA)
    )

  # Get a list of unique year_month combos and order them
  date_set <- unique(data$year_month)
  date_set <- date_set[!is.na(date_set)]
  dates <- date_set[order(date_set)]

  # Get the total number of unique months in data
  length_dates <- length(dates)

  # Run the period check
  period <- period_is_factor(period, length_dates)

  # Split the dates into time periods based on the number of months
  # over which to summarise the data
  date_unit <- split(dates, ceiling(seq_along(dates) / period))


  # Name the elements of the list based on the oldest and newest month present in it
  names(date_unit) <- lapply(date_unit, function(x) if(period == 1) as.character(as.Date(min(x))) else paste0(min(x),"_to_",max(x)))

  # Create a lookup from the list above
  lookup <- tibble::tibble(
    year_month = as.Date(unlist(date_unit)),
    period = rep(names(date_unit), lengths(date_unit))
  )

  # Join the lookup to data
  data <- data %>%
    dplyr::left_join(lookup, by = "year_month")

  #### Ethnicity ####

  # Force collapse ethnicity if ethnicity definition is 'officer' becuase
  # officer-defined ethnicity is only the 5 category ethnicity
  if(ethnicity_definition == "officer"){
    collapse_ethnicity <- TRUE
  }

  population_ests <- policedatR::get_population_estimates(data, collapse_ethnicity)

  # # Tidy up the ethnicity names in population estimates for refactoring
  ethnicity_names <- create_ethnicity_names(population_ests, ethnicity_definition)

  if(ethnicity_definition == "self"){
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

    # Apply aggregated ethnicity names to the bins
    ethnicity_map <- setNames(ethnicity_map, ethnicity_names)

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


  #### Analysis ####


  # First initialise an empty grid so we capture all combinations of area, ethnicity
  # and date. This ensures we capture 0 counts, i.e. combinations where there is
  # nothing to count. n() will only count what's in the data and doesn't know
  # about any missing combinations.

  # Get the area variable to plug in to the summarisation
  area_variable <- colnames(data)[1] # Can add in the other area variables after
  remaining_areas <- colnames(data)[2:which(colnames(data) == "rgn22nm")]

  # Get all unique values
  area_vals <- unique(data[[area_variable]])
  ethnicity_vals <- unique(data$ethnicity)
  period_vals <- unique(data$period)

  # Create the combinations
  all_combinations <- expand.grid(
    area = area_vals,
    ethnicity = ethnicity_vals,
    period = period_vals,
    stringsAsFactors = FALSE
  )

    # Rename the "area" column to the correct variable name
  colnames(all_combinations)[1] <- area_variable

  # Count the stops
  summarised_data <- data %>%
    dplyr::group_by(!!rlang::sym(area_variable), ethnicity, period) %>%
    dplyr::summarise(
      stopped = dplyr::n(),
    ) %>%
    dplyr::ungroup()


  # Join counts with expanded grid. This will create NAs where no data in stops
  # to then be converted to 0s. Then add in pop ests and calculate rates per 1k
  complete_data <- all_combinations %>%
    dplyr::left_join(summarised_data, by = c(area_variable, "ethnicity","period")) %>%
    dplyr::mutate(
      stopped = ifelse(is.na(stopped), 0, stopped)
    ) %>%
    dplyr::full_join(population_ests, by = c(area_variable, "ethnicity")) %>%
    dplyr::mutate(
      stop_rate_per_1000 = 1000 * (stopped / population)
    )

  #### Tidying ####

  # Add in area lookup

  # First just get msoas and above from lookup
  areas_above <- area_lookup %>%
    dplyr::select(c(!!rlang::sym(area_variable), remaining_areas)) %>%
    dplyr::distinct()

  # colnames(areas_above)[3]

  # Then join
  complete_data <- complete_data %>%
    dplyr::left_join(areas_above, by = area_variable) %>%
    # Locate the area variables at the left of the dataframe
    dplyr::relocate(c(colnames(areas_above)[2]:rgn22nm), .after = area_variable)

  # browser()
  if(!is.null(comparison)){
    ethnicity_1 <- comparison[1]
    ethnicity_2 <- comparison[2]

    temp_df <- complete_data %>%
      dplyr::filter(ethnicity %in% comparison) %>%
      dplyr::mutate(
        ethnicity = factor(ethnicity, levels = comparison)
      )

    # Define a function that runs a Poisson regression to get the IRR
    compute_irr <- function(df) {
      # Defensive: filter only 2 ethnicities
      if (nrow(df) < 2 || length(unique(df$ethnicity)) < 2 || sum(df$stopped) == 0) return(tibble::tibble(irr = NA, conf.low = NA, conf.high = NA))

      # Fit Poisson model: stopped ~ ethnicity + offset(log(population))
      model <- glm(
        stopped ~ ethnicity,
        offset = log(population),
        data = df,
        family = poisson()
      )

      browser()
      # Extract exponentiated coefficient (IRR for non-reference level)
      broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
        dplyr::filter(term != "(Intercept)") %>%
        dplyr::select(term, estimate, conf.low, conf.high, p.value) %>%
        dplyr::rename(irr = estimate)
      browser()
    }

    # Apply the IRR computation within each area and period
    irr_results <- temp_df %>%
      dplyr::group_by(!!rlang::sym(area_variable), period) %>%
      dplyr::filter(dplyr::n() == 2) %>%   # ensure two ethnicities per group
      tidyr::nest() %>%
      dplyr::mutate(irr_info =purrr::map(data, compute_irr)) %>%
      tidyr::unnest(irr_info) %>%
      dplyr::select(-data)



  }

  # browser()
  # !! I think here i may need to do something with id cols for hte case where there
  # are multiple dates
  comparison_data <- temp_df %>%
    tidyr::pivot_wider(names_from = ethnicity, values_from = c(stopped,population,stop_rate_per_1000)) %>%
    dplyr::left_join(irr_results, by = c(area_variable,"period"))

  #### Old code starts here ####
  # ethnicity_1 <- comparison[1]
  # ethnicity_2 <- comparison[2]
  #
  # # subset to ethnicities to compare
  # data_subset <- subset(data, ethnicity == ethnicity_1 | ethnicity == ethnicity_2)
  # # refactor. First in comparison is reference, second is treatment
  # data_subset$ethnicity <- factor(data_subset$ethnicity, levels = comparison)

  # Summarisation starts here

  # The code below creates a contingency table for each LAD and runs chi-square
  # and Fisher's exact tests on it.
  # It also creates an overall contingency table on which to base combined statistics.

  # data_subset$la_name[which(data_subset$la_name == "Rhondda Cynon Taf")] <- "Rhondda Cynon Taff"
  # data_subset$region[which(data_subset$region == "Yorkshire and The Humber")] <- "Yorkshire and the Humber"



  # # create dummy area variable
  # data_subset[,"area"] <- data_subset[,2]
  # areas <- unique(data_subset$area) %>% pull()
  # # initialise
  # all_results <- data.frame()
  # all_mats <- matrix(data = c(0,0,0,0), ncol = 2, nrow = 2)
  # all_dfs <- data.frame()
  # count <- 0
  #
  # # regional fork would be here
  #
  # for(i in 1:length(areas)){
  #   results_df <- data.frame() # initialise
  #   # get data for this area
  #   this_area <- unique(data_subset[which(data_subset$area == areas[i]), 2]) %>% pull()
  #   # county <- unique(data_subset[which(data_subset$la_name == las[i]), "county"])
  #   # region <- unique(data_subset[which(data_subset$la_name == las[i]), "region"])
  #   # country <- unique(data_subset[which(data_subset$la_name == las[i]), "country"])
  #   # force <- unique(data_subset[which(data_subset$la_name == las[i]), "force"])
  #
  #   for(j in 1:length(dates)){
  #     if(date == "by_year"){
  #       this_date <- dates[j]
  #       temp_df <- data_subset %>% # subset to la
  #         filter(area == this_area & year == this_date)
  #     }
  #     else if(date == "by_month"){
  #       this_date <- dates[j]
  #       temp_df <- data_subset %>% # subset to la
  #         subset(., area == this_area & year_month == this_date)
  #     }
  #     else if(date == "12_month_periods"){
  #       this_date <- dates[[j]]
  #       temp_df <- data_subset %>% # subset to la
  #         subset(., area == this_area & year_month >= this_date[1] & year_month <= this_date[2])
  #       # rename this date for variable name later
  #       this_date <- paste0("12 months to ", substr(dates[[j]][2], 1, 7))
  #     }
  #
  #     # need also to consider istuatoin where both are missing
  #     if(length(unique(temp_df$ethnicity)) == 2){ # &
  #       # !is.na(population_ests[which(population_ests$LAD == la), ethnicity_1]) &
  #       # !is.na(population_ests[which(population_ests$LAD == la), ethnicity_2])){ # check that there are stops for both white and black individuals
  #
  #       # collect stats
  #       temp_df <- temp_df %>%
  #         dplyr::group_by(ethnicity) %>%
  #         dplyr::summarise(
  #           stopped = dplyr::n()
  #         ) %>%
  #         dplyr::mutate(
  #           pop = c(as.numeric(population_ests[which(population_ests$area == this_area), ethnicity_1]),
  #                   as.numeric(population_ests[which(population_ests$area == this_area), ethnicity_2])),
  #           percentage = 100 * (stopped/pop),
  #           not_stopped = pop - stopped
  #         ) %>%
  #         as.data.frame()
  #     } else if(length(unique(temp_df$ethnicity)) == 1){
  #       # get the missing ethnicity if one is missing - why would tehre be a missing ehtnicity again? ah becuase tehre are no counts for it
  #       missing_ethnicity <- comparison[which(!(comparison %in% temp_df$ethnicity))]
  #       data_to_insert <- data.frame("ethnicity" = missing_ethnicity, "stopped" = 0)
  #
  #       temp_df <- temp_df %>%
  #         dplyr::group_by(ethnicity) %>%
  #         dplyr::summarise(
  #           stopped = dplyr::n()
  #         )
  #
  #       temp_df <- bind_rows(temp_df, data_to_insert) %>%
  #         dplyr::mutate(
  #           pop = c(as.numeric(population_ests[which(population_ests$area == this_area), ethnicity_1]),
  #                   as.numeric(population_ests[which(population_ests$area == this_area), ethnicity_2])),
  #           percentage = 100 * (stopped/pop),
  #           not_stopped = pop - stopped
  #         ) %>%
  #         as.data.frame()
  #     }
  #     # note here we need to also account for no ehtnicities
  #
  #     #anotehr condition to run stats only if both ethnictiues are presnet
  #     # this is the OR aprt
  #
  #     if(length(unique(temp_df$ethnicity)) == 2){ # &
  #
  #       row.names(temp_df) <- comparison
  #
  #       ethn_1 <- data.frame("ethnicity_1" = c("stopped" = temp_df[ethnicity_1, "stopped"],
  #                                              "not_stopped" = temp_df[ethnicity_1, "not_stopped"]))
  #
  #       ethn_2 <- data.frame("ethnicity_2" = c("stopped" = temp_df[ethnicity_2, "stopped"],
  #                                              "not_stopped" = temp_df[ethnicity_2, "not_stopped"]))
  #
  #       comp_mat <- as.matrix(cbind(ethn_2, ethn_1)) # matrix for crosstable
  #       comp_df <- as.data.frame(comp_mat) # df for custom rr function
  #
  #       if(sum(is.na(comp_mat)) == 0){ # if there are figures for all cells, run stats
  #         # use tryCatch to collect warning messages
  #         xtab <- tryCatch(gmodels::CrossTable(comp_mat, chisq = T, fisher = T, expected = T),
  #                          warning = function(w) return(list(gmodels::CrossTable(comp_mat, chisq = T, fisher = T, expected = T), w)))
  #         rr <- riskratio_from_df(comp_df, "Stop & Search")
  #         results_df <- data.frame(
  #           "date" = this_date,
  #           "area" = this_area,
  #           # "county" = county,
  #           # "region" = region,
  #           # "country" = country,
  #           # "force" = force,
  #           "ethnicity_2_stopped" = temp_df[ethnicity_2, "stopped"],
  #           "ethnicity_2_not_stopped" = temp_df[ethnicity_2, "not_stopped"],
  #           "ethnicity_2_population" = temp_df[ethnicity_2, "pop"],
  #           "ethnicity_2_stop_rate" = temp_df[ethnicity_2, "percentage"],
  #           "ethnicity_1_stopped" = temp_df[ethnicity_1, "stopped"],
  #           "ethnicity_1_not_stopped" = temp_df[ethnicity_1, "not_stopped"],
  #           "ethnicity_1_population" = temp_df[ethnicity_1, "pop"],
  #           "ethnicity_1_stop_rate" = temp_df[ethnicity_1, "percentage"],
  #           "or" = ifelse(is.list(xtab[[1]]),
  #                         xtab[[1]][["fisher.ts"]][["estimate"]][["odds ratio"]],
  #                         xtab[["fisher.ts"]][["estimate"]][["odds ratio"]]),
  #           "or_ci_low" = ifelse(is.list(xtab[[1]]),
  #                                xtab[[1]][["fisher.ts"]][["conf.int"]][1],
  #                                xtab[["fisher.ts"]][["conf.int"]][1]),
  #           "or_ci_upp" = ifelse(is.list(xtab[[1]]),
  #                                xtab[[1]][["fisher.ts"]][["conf.int"]][2],
  #                                xtab[["fisher.ts"]][["conf.int"]][2]),
  #           "rr" = rr$rr,
  #           "rr_ci_low" = rr$ci_low,
  #           "rr_ci_upp" = rr$ci_upp,
  #           "warning" = ifelse(is.list(xtab[[1]]), xtab[[2]][["message"]], NA))
  #
  #         all_mats <- all_mats + comp_mat
  #         count <- count + 1 # increase count of areas for which stats have been acquired
  #
  #       }
  #       else { # if there are missing values, don't run stats but still add frequency data
  #         results_df <- data.frame(
  #           "date" = this_date,
  #           "area" = this_area,
  #           # "county" = county,
  #           # "region" = region,
  #           # "country" = country,
  #           # "force" = force,
  #           "ethnicity_2_stopped" = temp_df[ethnicity_2, "stopped"],
  #           "ethnicity_2_not_stopped" = temp_df[ethnicity_2, "not_stopped"],
  #           "ethnicity_2_population" = temp_df[ethnicity_2, "pop"],
  #           "ethnicity_2_stop_rate" = temp_df[ethnicity_2, "percentage"],
  #           "ethnicity_1_stopped" = temp_df[ethnicity_1, "stopped"],
  #           "ethnicity_1_not_stopped" = temp_df[ethnicity_1, "not_stopped"],
  #           "ethnicity_1_population" = temp_df[ethnicity_1, "pop"],
  #           "ethnicity_1_stop_rate" = temp_df[ethnicity_1, "percentage"],
  #           "or" = NA,
  #           "or_ci_low" = NA,
  #           "or_ci_upp" = NA,
  #           "rr" = NA,
  #           "rr_ci_low" = NA,
  #           "rr_ci_upp" = NA,
  #           "warning" = NA)
  #       }
  #     }
  #       else { # if there is not data for both black and white, and/or there are missing values
  #       results_df <- data.frame(
  #         "date" = this_date,
  #         "area" = this_area,
  #         # "county" = county,
  #         # "region" = region,
  #         # "country" = country,
  #         # "force" = force,
  #         "ethnicity_2_stopped" = NA,
  #         "ethnicity_2_not_stopped" = NA,
  #         "ethnicity_2_population" = NA,
  #         "ethnicity_2_stop_rate" = NA,
  #         "ethnicity_1_stopped" = NA,
  #         "ethnicity_1_not_stopped" = NA,
  #         "ethnicity_1_population" = NA,
  #         "ethnicity_1_stop_rate" = NA,
  #         "or" = NA,
  #         "or_ci_low" = NA,
  #         "or_ci_upp" = NA,
  #         "rr" = NA,
  #         "rr_ci_low" = NA,
  #         "rr_ci_upp" = NA,
  #         "warning" = NA)
  #     }
  #
  #
  #     all_results <- rbind(all_results, results_df) # add results to all results
  #     all_dfs <- as.data.frame(all_mats)
  #     cat("\014")
  #     print(paste0(i, " of ", length(areas), " complete (", round(100 * (i / length(areas)),2 ), "%)"))
  #   }
  #
  # }
  #
  # # add names to ethnicity categories
  # names(all_results) <- sub("^ethnicity_1", ethnicity_1, names(all_results))
  # names(all_results) <- sub("^ethnicity_2", ethnicity_2, names(all_results))

  if(is.null(comparison)){
    return(complete_data)
  }
  else{
    return(comparison_data)
  }


}
