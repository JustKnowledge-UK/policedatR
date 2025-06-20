% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyse_records.R
\name{analyse_anything}
\alias{analyse_anything}
\title{Analyse anything (provisional name)}
\usage{
analyse_anything(
  data,
  analysis_variables,
  ethnicity_definition,
  collapse_ethnicity,
  period = 12
)
}
\arguments{
\item{data}{A tibble of stop data acquired using policedatR}

\item{analysis_variables}{A character vector of variables to be used
as grouping variables when counting. Order is important. Variables are nested
from left to right; the first element is the highest grouping level and the
final element is the lowest grouping level. Percentage denominators are the
sum of counts within the final grouping level. Valid values are: area, ethnicity,
period, object, outcome, age, gender, legislation. Use \code{show_analysis_variables()}
to see these values and brief explanation of to what they refer. See details for more info.}

\item{ethnicity_definition}{If ethnicity is included, what definition to use.
\code{'self'} for self-defined, \code{'officer'} for officer-defined.}

\item{collapse_ethnicity}{If ethnicity is included, should it be aggregated (TRUE)
or not (FALSE)? If \code{ethnicity_definition == 'officer'}, forced to TRUE.}

\item{period}{The number of months to use as a time period, e.g. 6 = 6-month-periods;
1 = monthly periods.}
}
\value{
A tibble containing counts and percentages of stops based on the grouping
described by \code{analysis_variables}.
}
\description{
Calculate counts and percentages of stops based on a combination of grouping
variables. \code{analysis_variables} defines an ordered set of grouping variables
for which counts will be produced. For example, \verb{analysis_variables = c("area","period","object")"}
will count the number of stops for each object of search within each area and
time period. The percentage denominator is the sum of counts within the final
grouping level. In the example above, the denominator would be the total number
of stops within each area-period combination. The options for defining \code{analysis_variables}
can be found using \code{show_analysis_variables()}.
}
\examples{
# Get data for Haringey
data <- policedatR::get_lad_data(list("lad22nm" = "Haringey"))
# Count the number of stops for each object of search within each area and
# time period
summarised_data <- analyse_anything(
                           data,
                           analysis_variables = c("area","period","object"),
                           period = 12)
}
