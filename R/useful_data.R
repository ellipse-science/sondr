#' US Counties Data
#'
#' A dataset containing information about US counties, such as area and other attributes.
#'
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{county_fips}{FIPS code of the county}
#'   \item{population_density}{Population density in square miles}
#'   \item{projection_vote_democrat}{ARIMA projection of democrat vote share in 2024}
#' }
#'
#' @usage data(us_counties_data)
#' @export
#' @docType data
#' @name us_counties_data
#' @keywords datasets
#'

#' US Counties Map Data
#'
#' A dataset containing geographic coordinates for the boundaries of US counties.
#'
#' This dataset provides the latitude and longitude points defining the boundaries of counties across the United States, identified by their respective FIPS codes.
#'
#' @format A tibble with 228,454 rows and 3 variables:
#' \describe{
#'   \item{long}{Numeric, the longitude of a point on the county boundary.}
#'   \item{lat}{Numeric, the latitude of a point on the county boundary.}
#'   \item{county_fips}{Character, the FIPS code of the county corresponding to the point.}
#' }
#'
#' @usage data(us_counties_map)
#' @export
#' @docType data
#' @keywords datasets
#' @name us_counties_map
