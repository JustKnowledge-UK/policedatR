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
                    "lad" = "lad22nm")

  area_lookup %>%
    dplyr::distinct(.data[[area_dict[[area_type]]]]) %>%
    dplyr::pull()
}
