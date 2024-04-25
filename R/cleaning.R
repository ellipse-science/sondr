#' Parse a Money Range
#'
#' This function parses a string containing a range of monetary values and returns a numeric vector of length two, where the first element is the lower bound and the second element is the upper bound of the range.
#'
#' @param value A character string containing the range of monetary values.
#' @param sep A character string containing the separator between the lower and upper bounds of the range. This parameter is required if `limit` is `NULL`.
#' @param limit A character string specifying how to handle values that do not have an explicit upper or lower bound. If `limit` is `"floor"`, the function assumes a lower bound of 0. If `limit` is `"ceiling"`, the function assumes an upper bound that is 30,000 units higher than the specified value. If `limit` is `NULL`, the function assumes that the value contains both lower and upper bounds, separated by `sep`.
#' @param ceiling_increment A numeric value specifying the increment to be added to the specified value when `limit` is `"ceiling"`. The default is 10,000.
#'
#' @return A numeric vector of length two, where the first element is the lower bound and the second element is the upper bound of the range.
#'
#' @examples
#' parse_money_range("$1,000 to $1,999", sep = "to")
#' parse_money_range("under $1,000", limit = "floor")
#' parse_money_range("$15,000 or over", limit = "ceiling")
#' parse_money_range("$15,000 or over", limit = "ceiling", ceiling_increment = 30000)
#'
#' @export
parse_money_range <- function(value, sep = NULL, limit = NULL, ceiling_increment = 10000){
  # Vérifier que les arguments sont valides
  if (!is.null(limit) && !limit %in% c("floor", "ceiling")) {
    stop("Invalid limit. Must be 'floor', 'ceiling', or NULL.")
  }
  if (is.null(limit)){
    if (is.null(sep)) {
      stop("sep must be provided if limit is NULL")
    }
    split <- strsplit(x = value, split = sep)[[1]]
    output <- as.numeric(gsub("[^0-9-]", "", split))
  } else {
    num <- as.numeric(gsub("[^0-9-]", "", value))
    if (limit == "floor"){
      output <- c(0, as.numeric(gsub("[^0-9-]", "", value)))
    } else if (limit == "ceiling"){
      num <- as.numeric(gsub("[^0-9-]", "", value))
      output <- c(num, num + ceiling_increment)
    }
  }
  return(output)
}


#' Clean Likert Numeric Vector
#'
#' Transforms raw Likert scale responses into a normalized numeric vector.
#' The transformation scales the data to a 0-1 range, based on the number of levels in the Likert scale.
#' This is particularly useful when Likert scale responses need to be treated as continuous variables
#' for statistical analysis.
#'
#' @param raw_vector A numeric vector containing raw Likert scale responses.
#' @return A numeric vector where each element is scaled to a 0-1 range.
#' @examples
#' # Example data
#' raw_data <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
#'
#' # Clean the data using clean_likert_numeric_vector
#' clean_data <- clean_likert_numeric_vector(raw_data)
#' print(clean_data)
#'
#' # Applying it to a data frame column example
#' data_clean$comp_sante_focus_attention_present <- clean_likert_numeric_vector(data_raw$autogestion_1)
#'
#' @export
clean_likert_numeric_vector <- function(raw_vector){
  clean_vector <- (raw_vector - 1) / (length(table(raw_vector)) - 1)
  return(clean_vector)
}



