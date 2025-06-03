#' Analyse anything (provisional name)
#'
#' Calculate counts and percentages of stops based on a combination of grouping
#' variables. `analysis_variables` defines an ordered set of grouping variables
#' for which counts will be produced. For example, `analysis_variables = c("area","period","object")"`
#' will count the number of stops for each object of search within each area and
#' time period. The percentage denominator is the sum of counts within the final
#' grouping level. In the example above, the denominator would be the total number
#' of stops within each area-period combination. The options for defining `analysis_variables`
#' can be found using `show_analysis_variables()`.
#'
#' @param data A tibble of stop data acquired using policedatR
#' @param analysis_variables A character vector of variables to be used
#' as grouping variables when counting. Order is important. Variables are nested
#' from left to right; the first element is the highest grouping level and the
#' final element is the lowest grouping level. Percentage denominators are the
#' sum of counts within the final grouping level. Valid values are: area, ethnicity,
#' period, object, outcome, age, gender, legislation. Use `show_analysis_variables()`
#' to see these values and brief explanation of to what they refer. See details for more info.
#' @param ethnicity_definition If ethnicity is included, what definition to use.
#' `'self'` for self-defined, `'officer'` for officer-defined.
#' @param collapse_ethnicity If ethnicity is included, should it be aggregated (TRUE)
#' or not (FALSE)? If `ethnicity_definition == 'officer'`, forced to TRUE.
#' @param period The number of months to use as a time period, e.g. 6 = 6-month-periods;
#' 1 = monthly periods.
#'
#' @returns A tibble containing counts and percentages of stops based on the grouping
#' described by `analysis_variables`.
#' @export
#'
#' @importFrom stats setNames
#' @examples

#' # Get data for Haringey
#' data <- policedatR::get_lad_data(list("lad22nm" = "Haringey"))

#' # Count the number of stops for each object of search within each area and
#' # time period
#' summarised_data <- analyse_anything(
#'                            data,
#'                            analysis_variables = c("area","period","object"),
#'                            period = 12)


analyse_anything <- function(data,
                             analysis_variables,
                             ethnicity_definition,
                             collapse_ethnicity,
                             period = 12){

  #### Helpers ####

  # This function takes input from user and maps it to variables in data and
  # then creates a data frame with every combination of the values in these
  # variables. This allows us to include combinations that have count values of
  # 0 which would otherwise be omitted by dplyr::n().
  initialise_grid <- function(data, analysis_variables){
    # Get area variable code from first column
    area_variable <- colnames(data)[1]

    # Create a mapping between the input arguments and the data variables
    groups_map <- list(
      "area" = area_variable,
      "ethnicity" = "ethnicity",
      "period" = "period",
      "object" = "object_of_search",
      "outcome" = "outcome",
      "age" = "age_range",
      "gender" = "gender",
      "legislation" = "legislation"
    )

    # Get an unnamed character vector of the actual variable names using
    # the mapping to translate user inputs into variables
    analysis_variables2 <- unname(unlist(groups_map[analysis_variables]))

    # Use the character vector to get unique values of each variable in a name
    # list
    values <- setNames(
      lapply(analysis_variables2, function(x) unique(data[[x]])),
      analysis_variables2
    )

    # Use the list to create a data frame where all combinations of the input
    # variables are present
    all_combinations <- do.call(expand.grid, c(values, stringsAsFactors = FALSE))

    # We also want to return the converted variables for indexing (there may
    # be a more elegant refactoring possible here)
    return(list(
      analysis_variables2 = analysis_variables2,
      all_combinations = all_combinations)
    )
  }


  # Process ethnicity. This creates nice names based on population estimates
  # and handles aggregation and types of definition.
  # Only do this if ethnicity is asked for.
  if("ethnicity" %in% analysis_variables){
    # Force collapse for officer-defined.
    if(ethnicity_definition == "officer"){
      collapse_ethnicity <- TRUE
    }
    data <- process_ethnicity(data,
                              collapse_ethnicity = collapse_ethnicity,
                              ethnicity_definition = ethnicity_definition)
  }

  # Create a period variable based on the value of period
  # Only if period is asked for
  if("period" %in% analysis_variables){
    data <- create_periods(data, period)
  }

  # Initialise a data frame containing all combinations of the analysis variables
  # This allows us to include combinations for which the count is 0 (as these are
  # omitted by n()) by left_joining later
  result <- initialise_grid(data, analysis_variables)
  all_combinations <- result$all_combinations
  analysis_variables2 <- result$analysis_variables2

  #### Analysis ####

  # Count the stops
  summarised_data <- data %>%
    dplyr::group_by(!!!rlang::syms(analysis_variables2)) %>%
    dplyr::summarise(
      n = dplyr::n(),
    ) %>%
    dplyr::mutate(
      percentage = 100 * (n / sum(n))
    ) %>%
    dplyr::ungroup()



  # Join counts with expanded grid. This will create NAs where no data in stops
  # to then be converted to 0s.
  complete_data <- all_combinations %>%
    dplyr::left_join(summarised_data, by = analysis_variables2) %>%
    dplyr::mutate(
      # Make NAs for n and percentage 0 - this is safe as NAs are a result of the left_join which
      # flags the combinations that weren't counted in the summarise step because
      # they didn't exist
      dplyr::across(c(n, percentage), ~ ifelse(is.na(.), 0, .))
    ) %>%
    # Make order the same as grouping - this was lost in the left_join above
    dplyr::arrange(!!!rlang::syms(analysis_variables2[-length(analysis_variables2)]))

  return(complete_data)
}

#' Calculate risk ratio
#'
#' Calculate risk ratio between the stop rates of two different ethnicities. Runs
#' policedatR::analyse_anything() with the grouping `(area, period, ethnicity)` to
#' produce counts for each area-period-ethnicity combination, then runs`epitools::riskratio()`
#' on the output to calculate the risk ratioand confidence interval of stop rates of specified
#' ethnicities for each area and period.
#' @param data A tibble of stop data acquired using policedatR. The first column
#' must be the area code variable of the geography of interest (e.g. 'lad22cd').
#' @param analysis_variables A character vector of variables to be used
#' as grouping variables when counting. Order is important. Variables are nested
#' from left to right; the first element is the highest grouping level and the
#' final element is the lowest grouping level. Percentage denominators are the
#' sum of counts within the final grouping level. Valid values are: area, ethnicity,
#' period, object, outcome, age, gender, legislation. Use `show_analysis_variables()`
#' to see these values and brief explanation of to what they refer. See details for more info.
#' @param ethnicity_definition String specifying which ethnicity definition to use
#' for counts. 'self' has the possibility of using the 18 disaggregated categories but is likely
#' to have more NAs than 'officer'. 'officer' is only the 5 aggregated ethnicity categories and
#' will likely have fewer NAs (technically it should have 0 but in practice this isn't the case)
#' @param collapse_ethnicity If `ethnicity_definition == 'self'`, this controls
#' whether to use the 18 disaggregated categories or to aggregate to the 5 broader
#' categories. If `ethnicity_definition == 'self'`, `collapse_ethnicity` is always TRUE.
#' @param comparison A character vector with length 2, where the first element is
#' the reference level of ethnicity against which the second ('test') level is compared.
#' If NULL, user will be prompted to pick the reference and test ethnicities.
#' @param period Numeric value specifying the number of months each time period
#' should be.
#'
#' @returns A tibble with number of stops and stop rate per 1000 population for each
#' ethnicity, and risk ratio and confidence interval within each area-period combination.
#' @export
#'
#' @examples
#' # Get data for Haringey and Lambeth
#' data <- policedatR::get_lad_data(list("lad22nm" = "Haringey"))
#'
#' # Calculate risk ratio between Black and White people (White is reference).
#' summarised_data <- calculate_riskratio(
#'                                      data,
#'                                      ethnicity_definition = "self",
#'                                      collapse_ethnicity = TRUE,
#'                                      comparison = c("white","black"),
#'                                      period = 12)
calculate_riskratio <- function(data,
                                analysis_variables = c("area","period","ethnicity"),
                                ethnicity_definition,
                                collapse_ethnicity,
                                comparison = c("white","black"),
                                period = NULL){

  # If period is NULL, set it to the total number of months present in data
  if(is.null(period)){
    # Make year_month variable
    data <- process_dates(data)
    # Get a list of unique year_month combos and order them
    date_set <- unique(data$year_month)
    date_set <- date_set[!is.na(date_set)]
    dates <- date_set[order(date_set)]
    # Get length of dates
    length_dates <- length(dates)
    # Set period
    period <- length(dates)
  }

  data <- create_periods(data, period)
  area_variable <- colnames(data)[1]
  all_area_variables <- colnames(data)[1:which(colnames(data) == "rgn22nm")]
  remaining_area_variables <- colnames(data)[2:which(colnames(data) == "rgn22nm")]

  population_ests <- policedatR::get_population_estimates(data, collapse_ethnicity)

  # Get the count data
  summarised_data <- policedatR::analyse_anything(data,
                                                  analysis_variables = analysis_variables,
                                                  ethnicity_definition = ethnicity_definition,
                                                  collapse_ethnicity = collapse_ethnicity,
                                                  period = period)  %>%
    dplyr::full_join(population_ests, by = c(area_variable, "ethnicity")) %>%
    dplyr::rename(
      stopped = n
    ) %>%
    dplyr::mutate(
      not_stopped = population - stopped,
      stop_rate_per_1000 = 1000 * (stopped / population)
    )

  # If comparison isn't specified, help user choose by listing categories
  # Note management needed here to ensure correct inputs.
  if(is.null(comparison)){
    ethnicities <- unique(levels(summarised_data$ethnicity))

    cat("\nEthnicities detected in data:\n")
    cat(paste(seq_along(ethnicities), ethnicities, sep = ". "), sep = "\n")
    ethn1 <- as.numeric(readline("Please select the number of the reference ethnicity: "))
    ethn2 <- as.numeric(readline("Please select the number of the test ethnicity: "))

    comparison <- ethnicities[c(ethn1, ethn2)]
    cat(paste0("\nComparing ", comparison[2], " stop rate to ", comparison[1], " stop rate..."))
  }

  ethnicity_1 <- comparison[1]
  ethnicity_2 <- comparison[2]

  # Subset to ethnicities of interest
  temp_df <- summarised_data %>%
    dplyr::filter(ethnicity %in% comparison) %>%
    dplyr::mutate(
      ethnicity = factor(ethnicity, levels = comparison)
    )

  # Run the risk ratio
  rr_results <- temp_df %>%
    dplyr::group_by(!!rlang::sym(area_variable), period) %>%
    dplyr::filter(dplyr::n_distinct(ethnicity) == 2) %>%   # ensure two unique ethnicities per group
    dplyr::select(!!rlang::sym(area_variable), period, ethnicity, not_stopped, stopped) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      # First ensure ethnicity_1 is the first row
      data = purrr::map(data, ~ .x %>% dplyr::arrange(dplyr::desc(ethnicity == ethnicity_1))),
      # Then run risk ratio
      rr_info = purrr::map(data, riskratio_from_df)) %>%
    tidyr::unnest(rr_info) %>%
    dplyr::select(-data)

  # Widen original data and join rr to it
  complete_data <- temp_df %>%
    tidyr::pivot_wider(id_cols = c(!!rlang::sym(area_variable), period), names_from = ethnicity, values_from = c(stopped,population,stop_rate_per_1000)) %>%
    dplyr::left_join(rr_results, by = c(area_variable,"period"))

  # Add in the other area variables
  all_areas <- area_lookup %>%
    dplyr::select(dplyr::all_of(all_area_variables)) %>%
    dplyr::distinct()

  # Join area variables to data and locate them sensibly
  complete_data <- complete_data %>%
    dplyr::left_join(all_areas, by = area_variable) %>%
    # Locate the area variables at the left of the dataframe
    dplyr::relocate(dplyr::all_of(remaining_area_variables), .after = area_variable)

  return(complete_data)

  }

