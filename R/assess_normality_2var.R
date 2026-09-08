#' @title Normality and Variance Assessment for Two Variables
#' @description This function generates a comprehensive graphical and statistical
#'   assessment of normality and variance for two numeric variables. For each
#'   variable, it produces a boxplot and a Normal Q-Q plot, and displays the
#'   results of a Shapiro-Wilk normality test. It also displays the results of
#'   a robust Levene's test for equal variances.
#' @param x1 A numeric vector representing the first variable to be assessed.
#' @param x2 A numeric vector representing the second variable to be assessed.
#' @param var_name1 A character string to be used as the name for the first
#'   variable in the plots. Defaults to "Data 1".
#' @param var_name2 A character string to be used as the name for the second
#'   variable in the plots. Defaults to "Data 2".
#' @return The function does not return a value. It generates a multi-panel
#'   plot to the current graphics device.
#' @details The function handles missing values (`NA`) by removing them prior
#'   to analysis. It also includes input validation to ensure that both
#'   variables are numeric and have at least 3 non-missing values. In addition
#'   to normality tests, it performs a Levene's test for the equality of
#'   variances for the two groups and displays the results.
#' @seealso \code{\link[stats]{shapiro.test}}, \code{\link[stats]{qqnorm}},
#'   \code{\link[graphics]{boxplot}}, \code{\link[graphics]{layout}},
#'   \code{\link[car]{leveneTest}}
#' @importFrom stats shapiro.test
#' @importFrom stats qqnorm
#' @importFrom stats qqline
#' @importFrom graphics boxplot
#' @importFrom graphics layout
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics text
#' @importFrom graphics legend
#' @importFrom stats rnorm
#' @importFrom stats rgamma
#' @importFrom car leveneTest # Added import for the Levene's test

#' @examples
#' # Example 1: Comparing a normal and a non-normal distribution
#' # Generate a normal dataset
#' set.seed(123)
#' data_normal <- stats::rnorm(100, mean = 50, sd = 10)
#'
#' # Generate a skewed dataset (non-normal)
#' data_skewed <- stats::rgamma(100, shape = 2, scale = 5)
#'
#' # Run the normality assessment
#' assess_normality_2var(
#'  x1 = data_normal,
#'  x2 = data_skewed,
#'  var_name1 = "Normal Distribution",
#'  var_name2 = "Skewed Distribution"
#' )
#'
#' # Example 2: Handling vectors with missing values
#' # Generate datasets with some missing values
#' data_with_na1 <- c(stats::rnorm(90), rep(NA, 10))
#' data_with_na2 <- c(stats::rnorm(90, 2), NA)
#'
#' # The function will automatically remove the missing values before analysis
#' assess_normality_2var(
#'  x1 = data_with_na1,
#'  x2 = data_with_na2,
#'  var_name1 = "Data with NAs 1",
#'  var_name2 = "Data with NAs 2"
#' )
#'
#' # Clean up graphical parameters after examples
#' if (!is.null(dev.list())) dev.off()

assess_normality_2var <- function(x1, x2, var_name1 = "Data 1", var_name2 = "Data 2") {
  
  # --- Input Validation ---
  if (!is.numeric(x1) || !is.numeric(x2)) {
    stop("Inputs 'x1' and 'x2' must be numeric vectors.")
  }
  
  x1 <- x1[!is.na(x1)]
  x2 <- x2[!is.na(x2)]
  
  if (length(x1) < 3 || length(x2) < 3) {
    stop("Input vectors must each have at least 3 non-missing values.")
  }
  
  # --- Helper Function to Plot a Single Variable's Assessment ---
  .plot_single_var <- function(x, var_name) {
    shapiro_test <- stats::shapiro.test(x)
    
    par(mar = c(4, 4, 2, 1))
    graphics::boxplot(x, main = paste("Boxplot for", var_name), 
                      col = "lightblue", border = "grey30", horizontal = TRUE)
    
    stats::qqnorm(x, main = paste("Normal Q-Q Plot for", var_name), pch = 16, col = "grey40")
    stats::qqline(x, col = "steelblue", lwd = 2)
    
    par(mar = c(0, 0, 0, 0))
    plot(0, 0, type = 'n', axes = FALSE, xlab = "", ylab = "")
    
    w_statistic <- sprintf("W = %.4f", shapiro_test$statistic)
    p_value_text <- sprintf("p-value = %.4f", shapiro_test$p.value)
    interpretation <- ifelse(shapiro_test$p.value > 0.05, 
                             "p > 0.05: Fail to reject H0\n(Data may be normal)",
                             "p <= 0.05: Reject H0\n (Data likely not normal)")
    
    graphics::legend("center", 
                     legend = c("Shapiro-Wilk Test:", w_statistic, p_value_text, "", interpretation),
                     bty = "n", 
                     cex = 1.2,
                     text.font = c(2, 1, 1, 1, 2))
  }
  
  # --- Perform Levene's Test ---
  combined_data <- data.frame(
    values = c(x1, x2),
    group = factor(c(rep(var_name1, length(x1)), rep(var_name2, length(x2))))
  )
  levene_test <- car::leveneTest(values ~ group, data = combined_data)
  
  # --- Set up the overall plotting layout for 2 variables ---
  layout(matrix(c(1, 1, 1, 2, 3, 4, 5, 6, 7, 8, 8, 8), nrow = 4, ncol = 3, byrow = TRUE),
         heights = c(0.5, 2, 2, 0.5))
  
  # --- PLOT 1: Main Title for the entire figure ---
  par(mar = c(0, 0, 2, 0))
  plot(0, 0, type = 'n', axes = FALSE, xlab = "", ylab = "")
  title_text <- "Normality and Variance Assessment"
  graphics::text(0, 0, title_text, cex = 1.8, font = 2)
  
  # --- Plot the assessment for the first variable ---
  .plot_single_var(x1, var_name1)
  
  # --- Plot the assessment for the second variable ---
  .plot_single_var(x2, var_name2)
  
  # --- Plot for the Levene's Test Results ---
  par(mar = c(0, 0, 0, 0))
  plot(0, 0, type = 'n', axes = FALSE, xlab = "", ylab = "")
  
  levene_f <- levene_test[1, 2]
  levene_p <- levene_test[1, 3]
  
  interpretation <- ifelse(levene_p > 0.05,
                           "Variances may be equal (p > 0.05)",
                           "Variances likely not equal (p <= 0.05)")
  
  result_text <- sprintf("Levene's Test for Homogeneity of Variance: F = %.3f, p = %.3f. %s.",
                         levene_f, levene_p, interpretation)
  
  graphics::text(0, 0, result_text, cex = 1.2, font = 2)
  
  # --- Reset graphical parameters to default ---
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  layout(1)
}