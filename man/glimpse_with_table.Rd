% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{glimpse_with_table}
\alias{glimpse_with_table}
\title{Glimpse Data Frame with Frequency Tables}
\usage{
glimpse_with_table(df, n_values = 5)
}
\arguments{
\item{df}{A data frame to be summarized.}

\item{n_values}{Integer. The number of unique values to display for character or factor columns. Default is 5.}
}
\value{
No return value, called for side effects (printing the data frame structure and summaries).
}
\description{
This function provides an overview of a data frame similar to `dplyr::glimpse()`, but with added frequency tables for character and factor columns. 
It displays the number of rows and columns, as well as the first few most frequent values for character or factor columns.
}
\details{
For each column, the function prints the column name, its data type, and a brief summary of its contents. 
For character or factor columns, it displays the frequency of the most common `n_values` values. 
For other column types, it displays the first few values using `head()`.
}
\examples{
df <- data.frame(
  x = sample(c("A", "B", "C"), size = 100, replace = TRUE),
  y = sample(c("D", "E", "F"), size = 100, replace = TRUE)
)
glimpse_with_table(df)

}
