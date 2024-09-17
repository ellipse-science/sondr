#' Create a stratified table with weighted counts and proportions
#'
#' This function generates a stratified table based on a specified geographic level and demographic variables,
#' using population weights to calculate both weighted counts and proportions.
#'
#' @param data A data frame containing individual census data.
#' @param strata_level A string representing the geographic or stratification level (e.g., state).
#' @param strata_vars A character vector of variable names to group by (e.g., demographic variables).
#' @param weight_var A string representing the column name of the weights to be used (e.g., "PERWT").
#'
#' @return A data frame with stratified weighted counts and proportion in the strata_level.
#' @examples
#' stratification_table(census_data, "ses_state", c("ses_gender", "ses_age_group", "ses_ownership"), "weight")
stratification_table <- function(data, strata_level = "ses_state", strata_vars, weight_var = "weight") {
  stratified_table <- data %>%
    tidyr::drop_na(all_of(c(strata_level, strata_vars))) %>%
    dplyr::group_by_at(c(strata_level, strata_vars)) %>%
    dplyr::summarise(weighted_count = sum(.data[[weight_var]], na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(proportion = weighted_count / sum(weighted_count))
  
  return(stratified_table)
}
