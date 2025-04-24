# List variables that are used with non-standard evaluation (e.g. in dplyr etc.)
# to suppress the "No visible binding for global variable" notes from check()
utils::globalVariables(c(
  "datetime",
  "geometry",
  "lad22cd",
  "lad22nm",
  "lsoa21cd",
  "lsoa21nm",
  "msoa21cd",
  "msoa21nm",
  "oa21cd",
  "pfa22cd",
  "pfa22nm",
  "rgn22cd",
  "rgn22nm",
  "shape_area"
))
