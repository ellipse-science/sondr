#' Robust CSV Reading Function
#'
#' `read_any_csv()` aims to robustly read a CSV file, navigating through common issues
#' such as different delimiters, problematic headers, and others. It employs various reading
#' methods in a series of attempts to successfully read the file.
#'
#' @description
#' This function works through a series of attempts to read a CSV file, employing
#' different strategies to navigate around typical issues that might cause
#' reading functions to fail. It uses both base R and `readr` functions, trying
#' different combinations of parameters to facilitate reading in different scenarios.
#'
#' @param file_path A string specifying the file path and name.
#' @param ... Additional arguments to be passed to the reading functions.
#'
#' @return
#' A data frame containing the data read from the CSV file.
#'
#' @details
#' The function proceeds through a series of attempts to read the CSV file:
#'   1. Standard reading with `read.csv`.
#'   2. Reading without headers using `read.csv`.
#'   3. Using utils::read.csv2
#'   4. Utilizing `readr::read_csv`.
#'   5. Reading with `read.table` and `fill = TRUE`.
#' If all attempts fail, the function stops and returns an error message.
#'
#' @section Handling Issues:
#' - If headers are problematic or have a different number of column names,
#'   it tries reading without headers.
#' - For differences in managing CSV files between `read.csv` and `read_csv`,
#'   it tries both.
#' - It attempts to fill empty columns, which can be problematic in some instances.
#'
#' @examples
#' \dontrun{
#' data <- read_any_csv("path/to/your/file.csv")
#' }
#'
#' @export
#'
#' @importFrom readr read_csv
read_any_csv <- function(file_path, ...) {
  # Ensure that the file exists
  if (!file.exists(file_path)) {
    stop("The file does not exist. Please check the file path.")
  }

  # Attempt 1: Try to read normally
  try({
    data <- utils::read.csv(file_path, ...)
    message("File read successfully with read.csv.")
    return(data)
  }, silent = TRUE)

  # Attempt 2: Try to read without headers
  try({
    message("Trying to read without headers using read.csv.")
    data <- utils::read.csv(file_path, header = FALSE, ...)
    # Set the first row as column names
    message("    Fixing dataframe names.")
    colnames(data) <- as.character(data[1, ])
    # Remove the first row
    data <- data[-1, ]
    return(data)
  }, silent = TRUE)

  # Attempt 3: With utils::read.csv2
  try({
    message("Trying to read with read.csv2")
    data <- utils::read.csv2(file_path, ...)
    return(data)
  }, silent = TRUE)

  # Attempt 4: Try with readr
  try({
    message("Trying with readr::read_csv.")
    data <- readr::read_csv(file_path, ...)
    return(data)
  }, silent = TRUE)

  # Attempt 5: Try to fill empty columns
  try({
    message("Trying with fill = TRUE using read.table.")
    data <- utils::read.table(file_path, sep = ",", header = TRUE, fill = TRUE, ...)
    return(data)
  }, silent = TRUE)

  # If all attempts fail, stop with an error message
  stop("All attempts to read the file failed. Please check the file format and path.")
}




#' Read Survey Data
#'
#' This function reads in survey data from a specified file, handling various file formats (csv, xlsx, and Sav).
#'
#' @param file A character string specifying the name of the file to be read. The file should have either a .csv, .xlsx, or .Sav file extension.
#'
#' @return
#' A data frame containing the contents of the file. The data type (e.g., character, numeric) of each column in the returned data frame will depend on the file content.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   data_csv <- read_survey("path/to/your/file.csv")
#'   data_xlsx <- read_survey("path/to/your/file.xlsx")
#'   data_sav <- read_survey("path/to/your/file.Sav")
#' }
#'
#' @seealso
#' \code{\link[utils]{read.csv2}}, \code{\link[readxl]{read_xlsx}}, \code{\link[haven]{read_sav}} for the functions used to read data based on file type.
#'
#' @importFrom utils read.csv2
#' @importFrom readxl read_xlsx
#' @importFrom haven read_sav
#'
#' @note
#' Ensure that the appropriate libraries (readxl, haven) are installed before using this function to read .xlsx or .Sav files respectively.
read_survey <- function(file){
  ## get file extension
  ext <- tools::file_ext(file)
  ## load data according to extension
  if (ext == "csv"){
    data <- read_any_csv(file, encoding = "UTF-8")
  } else if (ext == "xlsx"){
    data <- readxl::read_xlsx(file)
  } else if (ext == "Sav"){
    data <- haven::read_sav(file)
  }
  return(data)
}


#' Load variable from file
#'
#'This function takes a file and a variable name and returns the vector
#'containing the variable values.
#'
#' @param file A string containing the path to a file.
#' @param variable_name A string containing the name of the variable.
#'
#' @return A vector with the same number of rows as the file containing the variable values
#' @export
#'
#' @examples
#' # Create a temporary CSV file with example data
#' temp_file <- tempfile(fileext = ".csv")
#' writeLines(c("var1,var2,var3", "1,2,3", "4,5,6", "7,8,9"), temp_file)
#'
#' # Load the variable 'var2' from the temporary CSV file
#' load_variable(file = temp_file, variable_name = "var2")
load_variable <- function(file, variable_name){
  data <- read_survey(file)
  return(as.vector(data[[variable_name]]))
}


#' Generate Survey IDs
#'
#' This function generates survey IDs by combining a source ID with a sequence of numbers.
#'
#' @param n_respondents The number of respondents, i.e., the length of the sequence.
#' @param source_id A character string representing the source ID (survey).
#'
#' @return A character vector containing the generated survey IDs.
#'
#' @details
#' This function generates survey IDs by combining a source ID with a sequence of numbers.
#' For example, if the source ID is "ces65" and n_respondents is 3, the function will return
#' the vector "ces65.1", "ces65.2", "ces65.3".
#'
#' @examples
#' generate_survey_ids(n_respondents = 5, source_id = "ces65")
#'
#' @export
#'
generate_survey_ids <- function(n_respondents, source_id){
  survey_ids <- paste0(source_id, ".", 1:n_respondents)
  return(survey_ids)
}


#' Update a Main Vector with Updates by Matching Names
#'
#' This function updates a main vector with values from an updates vector by matching names.
#' It replaces values in the main vector with corresponding values from the updates vector
#' where the names are the same.
#'
#' @param main A vector to update.
#' @param updates A vector containing the updates to main.
#'
#' @return The updated \code{main} vector with values from the \code{updates} vector where names match.
#'
#' @examples
#' main <- c(a = 1, b = 2, c = 3, d = 4)
#' updates <- c(b = 22, c = 33)
#' updated_vector <- match_and_update(main, updates)
#'
#' @export
#'
match_and_update <- function(main, updates) {
  ## Find elements who need to be updated
  need_update <- names(main)[names(main) %in% names(updates)]
  ## Update the values of these elements
  main[need_update] <- updates[need_update]
  return(main)
}

#' Extract elements with names starting with a specific prefix
#'
#' This function extracts elements from a vector whose names start with a given prefix.
#'
#' @param vec A named vector.
#' @param prefix A string representing the prefix to match.
#' @return A named vector containing the elements whose names start with the given prefix.
#' @export
#' @examples
#' vec <- c(foo1 = 1, foo2 = 2, bar1 = 3, bar2 = 4)
#' extract_elements_with_prefix(vec, "foo")
extract_elements_with_prefix <- function(vec, prefix) {
  indices <- grep(paste0("^", prefix), names(vec))
  elements <- vec[indices]
  return(elements)
}
