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
  ## get file extension
  ext <- tools::file_ext(file)
  ## load data according to extension
  if (ext == "csv"){
    data <- read.csv(file, encoding = "UTF-8")
  } else if (ext == "xlsx"){
    data <- readxl::read_xlsx(file)
  } else if (ext == "Sav"){
    data <- haven::read_sav(file)
  }
  return(as.vector(data[[variable_name]]))
}
