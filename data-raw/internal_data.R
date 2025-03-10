# 2025-03-10
# Jolyon Miles-Wilson

# A script for compiling the internal data to be used in the package.
# Internal data is used by the package but not accessible to the user.


# Clean environment
rm(list=ls())

# Output Area boundaries
# Check if you already have the packages and install if not
# list the packages
packages <- c(
  'DBI',
  'dplyr',
  'sf'
)

# find and install the packages not installed
pkg_not_install <- packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_not_install, install.packages, dependencies = TRUE)

# library the packages
lapply(packages, library, character.only = TRUE)

# Database paramters
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "localhost",
  dbname = "JustKnowledge",
  port = "5432",
  user = keyring::key_list("JustKnowledge")[1,2],
  password = keyring::key_get("JustKnowledge","postgres")
)

# Query
query <- "SELECT * FROM oa21_boundaries"

# Run query, drop unwanted columns, transform to Coordinate Reference System
# used by the Police API
oa_geometries <- st_read(con, query = query) %>%
  select(-c(parent_script,created)) %>%
  sf::st_transform(., crs = '+proj=longlat +datum=WGS84')

# Add the data to sysdata.rda to be used by the package (but not accessible by
# user; cf exported data)
usethis::use_data(oa_geometries,
                  # add other data here
                  internal = TRUE)
