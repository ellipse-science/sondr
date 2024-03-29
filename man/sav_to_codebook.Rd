% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaning.R
\name{sav_to_codebook}
\alias{sav_to_codebook}
\title{Convert .sav Data to a Codebook}
\usage{
sav_to_codebook(data)
}
\arguments{
\item{data}{A data frame where each column represents a variable from the .sav file.
It's expected that this data frame has attributes for 'label' (to use as
question text) and 'labels' (to use as answer choices) for each variable.}
}
\value{
A data frame with columns for each variable name, the associated question text
        (or `NA` if not available), and a concatenated string of answer choices (or `NA`
        if not applicable). Each row corresponds to a variable from the input data frame.
}
\description{
This function takes a data frame (typically loaded from a .sav file) and generates
a codebook. The codebook includes the variable names, associated questions (if available),
and answer options for each variable. It's particularly useful for survey data where
each variable might have a set of predefined answer choices.
}
\examples{
# Load a .sav file (example file path)
# data <- haven::read_sav("path/to/your/datafile.sav")

# Generate the codebook
# codebook <- sav_to_codebook(data)

}
