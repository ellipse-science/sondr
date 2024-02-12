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
  # Check if the input is a dataframe
  if (!is.data.frame(df)) {
    stop("Error: Argument 'df' must be a dataframe")
  }
  
  # Calculate Cronbach's alpha for reliability assessment
  cronbachAlpha <- round(psych::alpha(df)$total$raw_alpha, 2)
  
  # Conduct factor analysis with the specified number of factors
  factAnalysis <- factanal(df, factors = nfactors)
  factorVarNames <- names(df)
  factorLoadings <- as.numeric(factAnalysis$loadings[, 1])
  factor1stEigen <- round(eigen(cor(df))$values[1], 2)
  
  # Create a plot of factor loadings
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
