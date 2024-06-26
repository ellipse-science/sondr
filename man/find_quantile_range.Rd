% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{find_quantile_range}
\alias{find_quantile_range}
\title{Find Quantile Ranges for Given Values}
\usage{
find_quantile_range(x, data)
}
\arguments{
\item{x}{A numeric vector of values for which the quantile ranges are to be found.}

\item{data}{A numeric vector representing the dataset within which to find the quantiles.}
}
\value{
A matrix where each row corresponds to a value in `x` and contains two elements:
        the lower and upper bounds of the quantile range (inclusive).
        The quantile bounds are returned as percentages (e.g., 0.20 for the 20th percentile).
}
\description{
This function calculates the range of quantiles that each given value covers in a specified dataset.
It determines the lowest and highest quantiles within which each value falls.
The quantiles are computed based on the percentiles from 0% to 100%.
}
\examples{
# Generate a random dataset
set.seed(123)
data <- runif(100, min = 0, max = 1000)

# Find the quantile range for multiple values
find_quantile_range(c(134, 300), data)

}
