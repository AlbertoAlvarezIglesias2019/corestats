#' @title Generate Diagnostic Plots for ANOVA Assumptions
#'
#' @description This function performs diagnostic tests for a one-way ANOVA, checking
#' for normality of residuals and homogeneity of variances using base R and `data.table` (no `car` package).
#' It then generates a combined plot featuring a histogram and a QQ plot of the studentized residuals,
#' with the test results annotated in the subtitle.
#'
#' @param data A data frame containing the variables.
#' @param variable A character string specifying the name of the dependent variable (the continuous variable).
#' @param by A character string specifying the name of the grouping variable (the categorical variable).
#'
#' @return A `patchwork` object containing the combined
#'   histogram and QQ plot of the residuals, with statistical test results
#'   in the subtitle.
#'
#' @references
#' \itemize{
#'   \item Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality (complete samples). \emph{Biometrika}, 52(3/4), 591-611.
#'   \item Levene, H. (1960). Robust tests for equality of variances. In I. Olkin (Ed.), \emph{Contributions to Probability and Statistics}. Stanford University Press.
#' }
#'
#' @seealso \code{\link[stats]{aov}}, \code{\link[stats]{shapiro.test}}
#'
#' @examples
#' # Create a sample data frame
#' data_df <- data.frame(
#'   len = c(4.2, 11.5, 7.3, 5.8, 6.4, 10, 11.2, 11.2, 5.2, 7,
#'           16.5, 16.5, 15.2, 17.3, 22.5, 17.3, 13.6, 14.5, 18.8, 15.5,
#'           23.6, 18.5, 33.9, 25.5, 26.4, 32.5, 26.7, 21.5, 23.3, 29.5),
#'   dose = factor(rep(c("Low", "Medium", "High"), each = 10))
#' )
#'
#' # Generate the residual plots
#' residual_plot <- rcs_anova_residual_plot(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose"
#' )
#' print(residual_plot)
#' @export
rcs_anova_residual_plot <- function(data, variable, by) {
  
  # Ensure input is a data.table
  dt <- data.table::as.data.table(data)
  
  formu <- as.formula(paste(variable, "~", by, sep = ""))
  aov_object <- stats::aov(formu, data = dt)
  
  # Extract studentized residuals
  residuals_val <- stats::rstudent(aov_object)
  
  # Perform Shapiro-Wilk Test for Normality
  shapiro_test <- stats::shapiro.test(residuals_val)
  
  # Perform Levene's Test (median-centered / Brown-Forsythe) using base R
  val_vec <- dt[[variable]]
  grp_vec <- factor(dt[[by]])
  
  group_medians <- tapply(val_vec, grp_vec, median, na.rm = TRUE)
  abs_devs <- abs(val_vec - group_medians[as.character(grp_vec)])
  
  lev_fit <- stats::anova(stats::lm(abs_devs ~ grp_vec))
  levene_pval_val <- lev_fit$`Pr(>F)`[1]
  
  interpretation_leven <- ifelse(
    levene_pval_val > 0.05,
    "(Variances may be equal)",
    "(Variances likely not equal)"
  )
  
  interpretation_shapi <- ifelse(
    shapiro_test$p.value > 0.05,
    "(Data may be normal)",
    "(Data likely not normal)"
  )
  
  # Format p-values for text annotation
  shapiro_pval <- pvformat(shapiro_test$p.value)
  levene_pval <- pvformat(levene_pval_val)
  
  # Create a data.table for plotting
  residuals_dt <- data.table::data.table(residuals = residuals_val)
  
  # Create the histogram of residuals
  hist_plot <- ggplot2::ggplot(residuals_dt, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(bins = 10, fill = "darkblue", color = "white", alpha = 0.7) +
    ggplot2::labs(
      title = "Histogram of Studentized Residuals",
      x = "Studentized Residuals",
      y = "Frequency"
    ) +
    ggplot2::theme_minimal()
  
  # Create the QQ plot of residuals
  qq_plot <- ggplot2::ggplot(residuals_dt, ggplot2::aes(sample = residuals)) +
    ggplot2::geom_qq(color = "darkblue") +
    ggplot2::geom_qq_line(linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = "QQ Plot of Studentized Residuals",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggplot2::theme_minimal()
  
  # Combine plots using patchwork and add text annotations
  combined_plot <- patchwork::wrap_plots(hist_plot, qq_plot) +
    ggplot2::labs(
      title = "ANOVA Diagnostic Plots",
      subtitle = paste(
        "Normality (Shapiro-Wilk): p =", shapiro_pval, interpretation_shapi,
        "\nEqual Variances (Levene's): p =", levene_pval, interpretation_leven
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5)
    )
  
  return(combined_plot)
}