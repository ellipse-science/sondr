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
    data <- utils::read.csv2(file, encoding = "UTF-8")
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
#' load_variable(file = "data.csv", variable_name = "var332") ## should return the vector containing the values of var332.
load_variable <- function(file, variable_name){
  data <- read_survey(file)
  return(as.vector(data[[variable_name]]))
}
