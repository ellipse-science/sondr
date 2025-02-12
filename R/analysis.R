#' Glimpse Data Frame with Frequency Tables
#'
#' This function provides an overview of a data frame similar to `dplyr::glimpse()`, but with added frequency tables for character and factor columns. 
#' It displays the number of rows and columns, as well as the first few most frequent values for character or factor columns.
#'
#' @param df A data frame to be summarized.
#' @param n_values Integer. The number of unique values to display for character or factor columns. Default is 5.
#'
#' @details
#' For each column, the function prints the column name, its data type, and a brief summary of its contents. 
#' For character or factor columns, it displays the frequency of the most common `n_values` values. 
#' For other column types, it displays the first few values using `head()`.
#'
#' @return No return value, called for side effects (printing the data frame structure and summaries).
#' @export
#' 
#' @examples
#' df <- data.frame(
#'   x = sample(c("A", "B", "C"), size = 100, replace = TRUE),
#'   y = sample(c("D", "E", "F"), size = 100, replace = TRUE)
#' )
#' glimpse_with_table(df)
#'
#' @export
glimpse_with_table <- function(df, n_values = 5) {
  
  # Function to generate colored text based on missing percentage
  colorize_missing <- function(pct_missing) {
    # Scale the percentage to 0–255 for red intensity
    intensity <- round(255 * (pct_missing / 100))
    # Ensure it stays within valid range
    intensity <- ifelse(intensity > 255, 255, ifelse(intensity < 0, 0, intensity))
    # Generate ANSI escape code for RGB color
    color <- sprintf("\033[38;2;%d;0;0m", intensity)  # Red color only
    reset <- "\033[0m"  # Reset color
    paste0(color, pct_missing, "% missing", reset)
  }

  n_rows <- nrow(df)
  if (n_rows > 1000000){
    message("This is a large dataset... Be patient because it make take a little while")
  }
  cat("\n")
  cat("Rows:", n_rows, "\n")
  cat("Columns:", ncol(df), "\n")

  table_list <- mapply(table, df, SIMPLIFY = FALSE)

  for (col in names(df)) {
    nas <- sum(is.na(df[[col]]))
    pct_missing <- round(nas / n_rows * 100, 1)
    if (pct_missing > 0){
      missing_message <- colorize_missing(pct_missing)
      cat("$ ", col, " <", class(df[[col]]), "> [", missing_message, "] ", sep = "")
    } else {
      cat("$ ", col, " <", class(df[[col]]), "> ", sep = "")
    }
    end <- ifelse(length(table(df[[col]])) > n_values, n_values, length(table(df[[col]])))
    # Utilise table pour afficher les fréquences des valeurs
    cat(paste0("[", names(table_list[[col]][1:end]), "=", table_list[[col]][1:end], "]", collapse = " "))
    cat("\n")
  }
}

#' Top-Down Factor Analysis
#'
#' Performs a top-down factor analysis on a given data frame, computes Cronbach's alpha for reliability, conducts a factor analysis to extract the loadings of the first factor, and plots the factor loadings along with annotations for Cronbach's alpha and the first eigenvalue.
#'
#' @param df A data frame where each column represents a variable to be included in the factor analysis.
#' @param nfactors The number of factors to extract. Default is 1.
#' @return A ggplot object showing the factor loadings plot with annotations for Cronbach's alpha and the first eigenvalue. The function also invisibly returns a list containing Cronbach's alpha, the first eigenvalue, and the factor loadings.
#' @examples
#' # Example usage:
#' # df <- data.frame(variable1 = rnorm(100), variable2 = rnorm(100))
#' # topdown_fa(df)
#' @export
#' @importFrom psych alpha
#' @importFrom ggplot2 ggplot aes coord_flip geom_bar geom_text geom_hline annotate scale_y_continuous theme_linedraw theme element_text margin
topdown_fa <- function(df, nfactors = 1) {
  # Y'a un peu de deepseek la dedans <3 
  # Check if the input is a dataframe
  if (!is.data.frame(df)) {
    stop("Error: Argument 'df' must be a dataframe")
  }

  # Calculate Cronbach's alpha for reliability assessment
  cronbachAlpha <- round(psych::alpha(df)$total$raw_alpha, 2)

  # Conduct factor analysis with the specified number of factors
  factAnalysis <- factanal(df, factors = nfactors)
  
  # Custom function to wrap text manually
  wrap_text <- function(text, width) {
    sapply(text, function(t) {
      # Split the text into chunks of 'width' characters
      chunks <- strsplit(t, paste0("(?<=.{", width, "})"), perl = TRUE)[[1]]
      # Combine chunks with '\n'
      paste(chunks, collapse = "\n")
    }, USE.NAMES = FALSE)
  }
  
  # Apply the custom wrapping function to variable names
  factorVarNames <- wrap_text(names(df), width = 15)
  
  factorLoadings <- as.numeric(factAnalysis$loadings[, 1])
  factor1stEigen <- round(eigen(cor(df))$values[1], 2)

  # Create a plot of factor loadings with wrapped names
  FAplot <- ggplot2::ggplot(data.frame(factorVarNames, factorLoadings), ggplot2::aes(x = factorVarNames, y = factorLoadings)) +
    ggplot2::coord_flip() +
    ggplot2::geom_bar(stat = "identity", colour = "black", fill = "black", size = 1, width = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = as.character(round(factorLoadings, 2))), vjust = 0.35, hjust = -0.3, size = 5) +
    ggplot2::geom_hline(yintercept = 0.3, colour = "gray", linetype = "longdash") +
    ggplot2::annotate("text", label = paste("Alpha de Cronbach =", as.character(cronbachAlpha)), x = 1.1, y = 1.28, size = 5) +
    ggplot2::annotate("text", label = paste("Première valeur propre =", as.character(factor1stEigen)), x = 0.75, y = 1.28, size = 5) +
    ggplot2::annotate("segment", x = 0.4, xend = 1.45, y = 1, yend = 1, colour = "black") +
    ggplot2::annotate("segment", x = 1.45, xend = 1.45, y = 1, yend = Inf, colour = "black") +
    ggplot2::scale_y_continuous(name = "\n Coefficients de saturation \n", limits = c(0, 1.55), breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
    ggplot2::xlab("\n") +
    ggplot2::theme_linedraw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 15, margin = ggplot2::margin(r = 5, l = 3)),
                   axis.title.y = ggplot2::element_text(size = 15),
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.title.x = ggplot2::element_text(hjust = 0.3, vjust = -0.17, size = 20),
                   panel.grid = ggplot2::element_blank())

  # Print the plot and additional information
  print(FAplot)
  print("What we want:")
  print(paste0("Alpha de Cronbach > 0.6 -> ", cronbachAlpha))
  print(paste0("Première Valeur Propre > 1 -> ", factor1stEigen))
  print(paste0("Tous les coefficients de saturation > 0.3"))

  # Prepare and return the results
  result <- list(
    cronbachAlpha = cronbachAlpha,
    factor1stEigen = factor1stEigen,
    factorLoadings = factorLoadings,
    FAplot = FAplot
  )

  return(result)
}
#' Check and Report Missing Values in DataFrame Columns
#'
#' This function processes a DataFrame, typically sourced from a Qualtrics .sav file, to identify and quantify
#' missing values across all columns. It specifically treats empty strings in text columns as NA, recalculates
#' missing values, and then groups variables by their names minus numeric and text suffixes to calculate the
#' count and percentage of missing data per group. It also generates a bar plot displaying the percentage of
#' missing data for variable groups where the percentage is greater than zero.
#'
#' This function requires the package clessnize, which is not available on CRAN but can be installed from GitHub. To install clessnize, run:
#' \code{devtools::install_github("clessn/clessnize")}
#'
#' @param data A DataFrame containing the data to be analyzed for missing values.
#'
#' @return A DataFrame with each variable group, the count of NAs, and the percentage of NAs.
#' Additionally, a bar plot is displayed showing the percentage of missing data for each variable group
#' with non-zero missing data.
#'
#' @importFrom dplyr select mutate across filter
#' @importFrom stringr str_replace
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip labs
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' data <- data.frame(
#'   ID_1_TEXT = c("apple", "", "banana"),
#'   Value_1 = c(1, 2, NA),
#'   ID_2_TEXT = c("", "orange", "melon"),
#'   Value_2 = c(3, 4, 5)
#' )
#' results <- check_na(data)
#' print(results)
#'
qualtrics_na_counter <- function(data) {
  # Handle missing values for text columns
  data_text_columns <- data %>%
    select(ends_with("_TEXT")) %>%
    mutate(across(everything(), na_if, ""))

  # Remove text columns from the original data
  data <- data %>%
    select(-ends_with("_TEXT"))

  # Combine back the text data columns
  data <- cbind(data, data_text_columns)

  # Detect and group variable names by removing numeric and text suffixes
  variable_groups <- names(data) %>%
    str_replace("_\\d+(_TEXT)?$", "") %>%
    unique()

  # Create a data frame to store the results
  data_na_results <- data.frame(variable = variable_groups, na_count = NA_integer_, na_percentage = NA_real_)

  # Calculate NA counts and percentages for each group
  data_na_results <- lapply(variable_groups, function(prefix) {
    data_group <- select(data, starts_with(prefix))
    total_rows <- nrow(data_group)

    # Calculate the number of rows with at least one non-NA value
    present_counts <- apply(data_group, 1, function(row) any(!is.na(row)))

    # Summarize results
    present_sum <- sum(present_counts)

    c(na_count = total_rows - present_sum, na_percentage = 100 * (total_rows - present_sum) / total_rows)
  }) %>%
    do.call(rbind.data.frame, .) %>%
    setNames(c("na_count", "na_percentage")) %>%
    cbind(variable = variable_groups)

  # Create the plot
  plot <- ggplot(data = filter(data_na_results, na_percentage > 0), aes(x = reorder(variable, na_percentage), y = na_percentage)) +
    geom_bar(stat = "identity", fill = "#e923c8") +
    coord_flip() +
    theme_classic() +
    labs(x = "Groupe de variable", y = "Pourcentage de données manquantes",
         title = "Pourcentage de données manquantes \npar groupe de variable")

  # Print the plot explicitly
  print(plot)

  # Return only the data frame
  return(data_na_results)
}


#' Find Quantile Ranges for Given Values
#'
#' This function calculates the range of quantiles that each given value covers in a specified dataset.
#' It determines the lowest and highest quantiles within which each value falls.
#' The quantiles are computed based on the percentiles from 0% to 100%.
#'
#' @param x A numeric vector of values for which the quantile ranges are to be found.
#' @param data A numeric vector representing the dataset within which to find the quantiles.
#'
#' @return A matrix where each row corresponds to a value in `x` and contains two elements:
#'         the lower and upper bounds of the quantile range (inclusive).
#'         The quantile bounds are returned as percentages (e.g., 0.20 for the 20th percentile).
#'
#' @examples
#' # Generate a random dataset
#' set.seed(123)
#' data <- runif(100, min = 0, max = 1000)
#'
#' # Find the quantile range for multiple values
#' find_quantile_range(c(134, 300), data)
#'
#' @export
find_quantile_range <- function(x, data) {
  # Calculate the quantiles from 0% to 100% with a step of 1%
  quantiles <- quantile(data, probs = seq(0, 1, by = 0.01))
  # Function to find quantile range for a single value
  find_range <- function(value) {
    lower_bound <- max(which(quantiles <= value))
    upper_bound <- min(which(quantiles >= value))
    if (lower_bound - upper_bound > 1) {
      new_lower_bound <- upper_bound
      new_upper_bound <- lower_bound
      vector <- c(new_lower_bound, new_upper_bound) / 100
    } else {
      vector <- c(lower_bound, upper_bound) / 100
    }
    return(vector)
  }
  # Apply function over vector x
  ranges <- t(sapply(x, find_range))
  colnames(ranges) <- c("Lower Quantile", "Upper Quantile")
  return(ranges)
}
