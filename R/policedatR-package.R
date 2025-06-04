#' policedatR: A package for acquiring, augmenting and analysing stop and search data
#'
#' @description
#' An R package for acquiring, augmenting and analysing stop and search data from the UK Police API.
#' This package provides tools to fetch UK police stop and search data, perform geographical
#' analysis, and calculate ethnic disparities with population-adjusted risk ratios.
#'
#' @section Main Functions:
#'
#' **Data Acquisition Functions:**
#' * \code{\link{get_lad_data}()}: Get data by Local Authority District
#' * \code{\link{get_lsoa_data}()}: Get data by Lower Super Output Area
#' * \code{\link{get_msoa_data}()}: Get data by Middle Super Output Area
#' * \code{\link{get_oa_data}()}: Get data by Output Area
#' * \code{\link{get_pfa_data}()}: Get data by Police Force Area
#' * \code{\link{get_region_data}()}: Get data by Region
#'
#' **Analysis Functions:**
#' * \code{\link{analyse_anything}()}: Calculate counts and percentages of stops by grouping variables
#' * \code{\link{calculate_riskratio}()}: Calculate ethnic disparity risk ratios using Census population data
#'
#' **Helper Functions:**
#' * \code{\link{area_variables}()}: List available area variable options
#' * \code{\link{list_areas}()}: Show values for specific area variables
#' * \code{\link{show_analysis_variables}()}: Display available analysis grouping variables
#'
#' **Geometry Functions:**
#' * \code{\link{get_lad_geometries}()}, \code{\link{get_lsoa_geometries}()}, etc.: Get spatial boundaries
#'
#' **Utility Functions:**
#' * \code{\link{get_population_estimates}()}: Get Census 2021 population estimates
#' * \code{\link{newest_data}()}, \code{\link{oldest_data}()}: Check data availability dates
#' * \code{\link{flush_cache}()}: Clear cached data
#'
#' @section Typical Workflow:
#'
#' 1. **Explore available areas**: Use \code{area_variables()} and \code{list_areas()}
#' 2. **Acquire data**: Use appropriate \code{get_*_data()} function with geographic subset
#' 3. **Analyze data**: Use \code{analyse_anything()} for descriptive statistics
#' 4. **Calculate disparities**: Use \code{calculate_riskratio()} for ethnic disparity analysis
#'
#' @section Examples:
#'
#' \preformatted{
#' # Get data for Greater London Local Authority Districts
#' london_data <- get_lad_data(subset = list("rgn22nm" = "London"))
#'
#' # Analyze stops by area, period, and ethnicity
#' ethnicity_summary <- analyse_anything(
#'   london_data,
#'   analysis_variables = c("area", "period", "ethnicity"),
#'   ethnicity_definition = "self",
#'   collapse_ethnicity = TRUE,
#'   period = 12
#' )
#'
#' # Calculate risk ratio comparing Black vs White stop rates
#' disparity <- calculate_riskratio(
#'   london_data,
#'   ethnicity_definition = "self",
#'   collapse_ethnicity = TRUE,
#'   comparison = c("white", "black"),
#'   period = 12
#' )
#' }
#'
#' @section Data Sources:
#'
#' * **Police Data**: data.police.uk API (Open Government Licence v3.0)
#' * **Geographic Boundaries**: ONS Open Geography Portal (Open Government Licence v3.0)
#' * **Population Data**: Census 2021 via NOMIS (Open Government Licence v3.0)
#'
#' @section Installation:
#'
#' \preformatted{
#' # Install from GitHub
#' devtools::install_github("JustKnowledge-UK/policedatR")
#' library(policedatR)
#' }
#'
#' @section Key Features:
#'
#' * **Geographic flexibility**: Support for multiple administrative geography levels
#' * **Temporal analysis**: Flexible time period groupings
#' * **Ethnicity analysis**: Both self-defined and officer-defined ethnicity options
#' * **Population adjustment**: Risk ratio calculations using Census 2021 data
#' * **Caching**: Efficient data storage to minimize API calls
#' * **Spatial support**: Integration with sf package for geographic analysis
#'
#' @section Analysis Variables:
#'
#' Available grouping variables for analysis include:
#' * **area**: Geographic area names
#' * **period**: Time periods (configurable length)
#' * **ethnicity**: Self-defined or officer-defined ethnic categories
#' * **object**: Object of search (e.g., drugs, weapons)
#' * **outcome**: Search outcome (e.g., arrest, no further action)
#' * **age**: Age ranges
#' * **gender**: Gender categories
#' * **legislation**: Legal powers used
#'
#' Use \code{show_analysis_variables()} for detailed descriptions.
#'
#' @section Package Dependencies:
#'
#' This package relies on several key dependencies:
#' * **Data manipulation**: dplyr, tidyr, purrr
#' * **API interaction**: httr
#' * **Spatial analysis**: sf
#' * **Statistical analysis**: epitools
#' * **Caching**: cachem, memoise
#'
#' @author Jolyon Miles-Wilson \email{jolyon.miles-wilson@justknowledge.org.uk}
#' @author Celestin Okoroji \email{celestin.okoroji@justknowledge.org.uk}
#'
#' @seealso
#' * Package repository: \url{https://github.com/JustKnowledge-UK/policedatR}
#' * UK Police API: \url{https://data.police.uk/api/}
#' * ONS Geography Portal: \url{https://geoportal.statistics.gov.uk/}
#'
#' @keywords internal
"_PACKAGE"