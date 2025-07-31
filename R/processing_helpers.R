# Helper to process and clean chunks for subset API requests
process_chunk <- function(response) {
  geojson_data <- httr::content(response, as = "text")
  sf::st_read(geojson_data, quiet = TRUE)
}


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
    cat(paste0("\nAnalysing in ",period,"-month periods."))
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
    }
    else{
      if(period == length_dates){
        cat(paste0("\nAnalysing across all months in data (",period,") as one."))
      }
    }
  }
}

# Process dates into year-month format
#
# Process date and create a year_month variable
#
# @param data A tibble of stop data acquired using policedatR
#
# @returns A mutated version of data which contains a formatted year_month variable.
# @keywords internal
#
# @examples

process_dates <- function(data){
  # Prepare the dates for the period check
  # separate date into separate columns
  data$year <- as.numeric(substr(data$datetime, 1, 4))
  data$month <- as.numeric(substr(data$datetime, 6, 7))
  data$day <- as.numeric(substr(data$datetime, 9, 10))

  # make year_month variable for indexing - na problems here?
  # This is redundant now that year_month is applied at extraction
  # data <- data %>%
  #   dplyr::mutate(
  #     year_month = dplyr::if_else(!is.na(year),
  #                                 as.Date(paste(
  #                                   as.character(data$year),
  #                                   as.character(data$month),"01", sep= "-"), format = "%Y-%m-%d"), NA)
  #   )

  return(data)

}

# Create analysis periods from date
#
# Process date and create a period variable based on the user's specified period
# Calls `period_is_factor` to try to make sensible divisions based on the factors
# of period
#
# @param data A tibble of stop data acquired using policedatR
# @param period Numeric value specifying the number of months each time period
# should be.
#
# @returns A mutated version of data which contains a formatted period variable.
# @keywords internal
#
# @examples

create_periods <- function(data, period){
  # Prepare the dates for the period check
  # separate date into separate columns
  data <- process_dates(data)

  # Get a list of unique year_month combos and order them
  date_set <- unique(data$year_month)
  date_set <- date_set[!is.na(date_set)]
  dates <- date_set[order(date_set)]

  # Get the total number of unique months in data
  length_dates <- length(dates)

  # Run the period check
  period_is_factor(period, length_dates)

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

  return(data)
}


# This function takes the ethnicity levels from population estimates which
# can then be used to remap the ethnicity levels in the police data. This
# facilitates joining the two in the analysis step.
# @param data Tibble acquired using policedatR
# @param ethnicity_definition Character specifying self-defined (`'self'`) or officer-defined ethnicity (`'officer'`)
# @param collapse_ethnicity Boolean indicating whether to aggregate ethnicity (`TRUE`) or not (`FALSE`)
#
# @returns The same tibble that was put in with a new `ethnicity` which is a tidied
# version of either self- or officer-defined ethnicity.
#
# @keywords internal
#
# @examples
#
# data <- process_ethnicity(data, ethnicity_definition = "self", collapse_ethnicity = TRUE)

#' @importFrom stats setNames

process_ethnicity <- function(data, ethnicity_definition, collapse_ethnicity, population_ests = NULL){
  # If officer-defined force collapse_ethnicity TRUE. This should be redundant
  # as it will be specified in the script calling this function.
  if(ethnicity_definition == "officer"){
    collapse_ethnicity <- TRUE
  }

  # Get populations estimates - this is only necessary for getting the names
  # Perhaps there's a better way to do this.
  # Only do it if we don't already have them from the script using this function
  if(is.null(population_ests)){
    population_ests <- policedatR::get_population_estimates(data, collapse_ethnicity)
  }

  # Get names
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

  # Now map the self-defined ethnicity names in the data onto the ethnicity names we've
  # created
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


    } else{
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
        ethnicity = self_defined_ethnicity, .after = self_defined_ethnicity
      )
  }
  # If officer-defined, just use officer-defined
  else if(ethnicity_definition == "officer"){
    data <- data %>%
      dplyr::mutate(
        officer_defined_ethnicity = stringr::str_to_lower(officer_defined_ethnicity),
        ethnicity = officer_defined_ethnicity, .after = officer_defined_ethnicity
      )
  }
  return(data)
}

#' Show analysis variables
#'
#' Show what values can be used in `analysis_variables` argument of analysis
#' functions and a description of what each value means.
#'
#' @returns A tibble where `key` is the value to be input into `analysis_variables`
#' and `description` is a description of what it means
#' @export
#'
#' @examples
#'
#' policedatR::show_analysis_variables()
#'
show_analysis_variables <- function(){
  # Create a mapping between the input arguments and the data variables
  groups_map <- list(
    "area" = "The geographic area, e.g. local authority. Inherits from data.",
    "ethnicity" = "The ethnicity of the person stopped (follows definition and aggregation specified by user).",
    "period" = "The time period as specified in the analysis function call",
    "object" = "The item the Police report was being sought",
    "outcome" = "The result of the stop and search",
    "age" = "The age range of the person stopped",
    "gender" = "The gender of the person stopped",
    "legislation" = "The legislation used to make the stop"
  )

  tibble::tibble(
    key = names(groups_map),
    description = unlist(groups_map)
  )
}

# Risk ratio from data frame
#
# Calculates relative risk ratio and confidence intervals from a
# contingency table constructed for analysis of stop and search disparities
#
# @param df Contingency table as data frame on which to calculate risk ratio
# @param name Desired name of resultant data frame containing statistics
#
# @return A data frame containing risk ratio and confidence intervals
#
# @keywords internal
#
# @examples
#
# rr <- riskratio_from_df(df)
#
riskratio_from_df <- function(df){

  mat <- df %>%
    dplyr::select(-ethnicity) %>%
    as.matrix()

  # Use tryCatch to handle errors
  rr <- tryCatch({
    epitools::riskratio(mat)
  }, error = function(e) {
    # Return NULL if there's an error
    return(NULL)
  })

  if(is.null(rr)){
    # Return NA values if riskratio failed
    df_out <- data.frame("rr" = NA_real_,
                         "ci_low" = NA_real_,
                         "ci_upp" = NA_real_,
                         "p" = NA_real_)
  } else {

  # output into formatted data frame
    df_out <- data.frame("rr" = rr[["measure"]][2,1],
                         "ci_low" = rr[["measure"]][2,2],
                         "ci_upp" = rr[["measure"]][2,3],
                         "p" = rr[["p.value"]][2,2]) # this is fisher.exact p.value
  }
  return(df_out)
}




