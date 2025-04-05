#' List available areas
#'
#' List unique values of the different area types that could be used to get
#' data from the Police API.
#'
#' @param area_type A string definiting the area type for which distinct values
#' are sought. Options are "oa" (Output Area), "lsoa", (Lower layer Super Output
#' area), "msoa" (Middle layer Super Output Area), and "lad" (Local Authority
#' District)
#'
#' @returns A character vector of the unique values of area_type.
#' @export
#'
#' @examples
#'
#' list_areas(area_type = "lad")

list_areas <- function(area_type = "lad"){
  area_dict <- list("oa" = "oa21cd",
                    "lsoa" = "lsoa21nm",
                    "msoa" = "msoa21nm",
                    "lad" = "lad22nm",
                    "region" = "rgn22nm",
                    "pfa" = "pfa22nm"
                    )

  area_lookup %>%
    dplyr::distinct(.data[[area_dict[[area_type]]]]) %>%
    dplyr::pull()
}


#' Show area variable names
#'
#' Display the variable names of areas. Useful for checking what variables can
#' be used for subsetting requests.
#'
#' @returns A character vector of area variable names from the area lookup.
#'
#' @export
#'
#' @examples
#'
#' # List the areas
#' area_variables()
#'
area_variables <- function(){
  colnames(area_lookup)
}
