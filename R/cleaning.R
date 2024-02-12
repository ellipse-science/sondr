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
#' Invert the Order of Unique Values in a Vector
#'
#' This function inverts the order of unique numerical values in a vector. For each unique value in the input vector, 
#' it assigns a new value that maintains the original spacing but in reverse order. 
#' It's useful for transforming data in a way that preserves the relative magnitude of values while reversing their order.
#'
#' @param vec_col A numeric vector containing the values to be inverted. 
#'
#' @return A numeric vector of the same length as `vec_col`, with the order of unique values inverted. 
#'         The spacing between different values is preserved in the inverted order.
#'
#' @examples
#' finverser(c(1, 2, 3, 2, 1)) # Returns c(3, 2, 1, 2, 3)
#' finverser(c(10, 20, 30))    # Returns c(30, 20, 10)
#'
#' @export
finverser <- function(vec_col){
  # Extract unique and non-NA values from the input vector
  unique_col <- unique(vec_col)
  unique_col <- unique_col[!is.na(unique_col)]
  
  # Determine the number of unique values and the maximum value for later adjustment
  n <- length(unique_col)
  max <- max(vec_col, na.rm = TRUE)
  
  # Sort the unique values in ascending order and then reverse that order
  ord <- sort(as.vector(unique_col))
  rev <- rev(ord)
  
  # Replace each value in the input vector with its inverted counterpart
  for (i in 1:n){
    vec_col[vec_col == ord[i]] <- max + rev[i] 
  }
  
  # Adjust the inverted values to maintain the original spacing but in reverse order
  vec_col <- vec_col - max
  
  return(vec_col)
}

