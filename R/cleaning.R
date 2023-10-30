#' Parse a Money Range
#'
#' This function parses a string containing a range of monetary values and returns a numeric vector of length two, where the first element is the lower bound and the second element is the upper bound of the range.
#'
#' @param value A character string containing the range of monetary values.
#' @param sep A character string containing the separator between the lower and upper bounds of the range. This parameter is required if `limit` is `NULL`.
#' @param limit A character string specifying how to handle values that do not have an explicit upper or lower bound. If `limit` is `"floor"`, the function assumes a lower bound of 0. If `limit` is `"ceiling"`, the function assumes an upper bound that is 30,000 units higher than the specified value. If `limit` is `NULL`, the function assumes that the value contains both lower and upper bounds, separated by `sep`.
#'
#' @return A numeric vector of length two, where the first element is the lower bound and the second element is the upper bound of the range.
#'
#' @examples
#' parse_money_range("$1,000 to $1,999", sep = "to")
#' parse_money_range("under $1,000", limit = "floor")
#' parse_money_range("$15,000 or over", limit = "ceiling")
#'
#' @export
parse_money_range <- function(value, sep = NULL, limit = NULL){
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
      output <- c(num, num + 30000)
    }
  }
  return(output)
}
