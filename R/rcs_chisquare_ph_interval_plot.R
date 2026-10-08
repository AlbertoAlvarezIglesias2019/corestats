#' @title Generate a Forest Plot for Adjusted Differences in Proportions
#'
#' @description This function creates a forest plot to visualize the pairwise
#' differences in proportions between levels of a categorical variable using base R and `data.table` (no `dplyr`).
#' The plot displays the point estimates and their corresponding confidence intervals, which
#' are adjusted for multiple comparisons using the Bonferroni method.
#'
#' @param data A data frame containing the two categorical variables.
#' @param ccc A character string specifying the name of the first categorical variable (column variable).
#' @param rrr A character string specifying the name of the second categorical variable (row variable).
#'
#' @return A `ggplot` object representing the forest plot.
#'
#' @examples
#' # Create a sample data frame
#' data_df <- data.frame(
#'   level = factor(sample(c("A", "B", "C"), size = 100, replace = TRUE)),
#'   treatment = factor(sample(c("Treat", "Control", "Other"), size = 100, replace = TRUE))
#' )
#'
#' # Generate the forest plot
#' plot_intervals <- rcs_chisquare_ph_interval_plot(
#'   data = data_df,
#'   ccc = "treatment",
#'   rrr = "level"
#' )
#'
#' # Print the plot
#' print(plot_intervals)
#' @export
rcs_chisquare_ph_interval_plot <- function(data, ccc, rrr) {
  
  if (is.function(data) || missing(data)) {
    stop("The 'data' argument must be a valid data frame or data.table.")
  }
  
  # Call the helper function to get Bonferroni-adjusted intervals
  fit <- getintervals_bc(data, ccc, rrr)
  out <- data.table::as.data.table(fit)
  
  # Construct contrast and set factor levels in reverse order for correct display orientation
  cctt <- paste(out$group1, out$group2, sep = " - ")
  out[, contrast := cctt]
  out[, contrast := factor(contrast, levels = rev(cctt))]
  out <- out[, .(contrast, estimate, conf.low, conf.high)]
  
  # Get the first level of the row variable for axis labeling
  first_level <- levels(factor(data[[rrr]]))[1]
  
  # Create the forest plot
  plot_intervals <- ggplot2::ggplot(out, ggplot2::aes(x = estimate, y = contrast)) +
    # Add horizontal lines for confidence intervals
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low, xmax = conf.high), 
                            height = 0.2, linewidth = 1.2) +
    # Add points for the estimate
    ggplot2::geom_point(size = 3) +
    # Add a vertical reference line at 0
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "blue", linewidth = 2) +
    # Define labels and titles with large font size
    ggplot2::labs(
      title = "Forest Plot of Group Differences",
      subtitle = "Confidence intervals (with Bonferroni corrections)",
      x = paste("Difference in Proportions (", rrr, " = ", first_level, ")", sep = ""),
      y = "Contrast"
    ) +
    # Customize the theme to increase font sizes
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 22, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 18),
      axis.title = ggplot2::element_text(size = 20),
      axis.text = ggplot2::element_text(size = 22),
      legend.title = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 16)
    )
  
  return(plot_intervals)
}