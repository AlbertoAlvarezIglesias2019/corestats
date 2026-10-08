#' @title Generate a Forest Plot for Adjusted Differences in Proportions
#'
#' @description This function creates a forest plot to visualize the pairwise
#' differences in proportions using base R and `data.table` (no `dplyr`).
#' The plot displays the point estimates and their corresponding confidence intervals.
#'
#' @param pocu A data frame containing the results (typically the `data` element returned from `rcs_chisquare_intervals()`).
#'
#' @return A `ggplot` object representing the forest plot.
#'
#' @examples
#' # Create sample data frame representing proportion comparison results
#' pocu_data <- data.frame(
#'   Lab = c("Group A - Group B", "Group A - Group C", "Group B - Group C"),
#'   pe = c(0.15, 0.25, 0.10),
#'   lb = c(0.02, 0.10, -0.05),
#'   ub = c(0.28, 0.40, 0.25),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Generate the forest plot
#' plot_intervals <- rcs_chisquare_intervals_plot(pocu = pocu_data)
#' print(plot_intervals)
#' @export
rcs_chisquare_intervals_plot <- function(pocu) {
  
  if (is.function(pocu) || missing(pocu) || is.null(pocu)) {
    stop("The 'pocu' argument must be a valid data frame.")
  }
  
  dt <- data.table::as.data.table(pocu)
  
  # Select and rename columns using data.table syntax
  out <- dt[, .(contrast = Lab, estimate = pe, conf.low = lb, conf.high = ub)]
  
  # Create the forest plot
  plot_intervals <- ggplot2::ggplot(out, ggplot2::aes(x = estimate, y = contrast)) +
    # Add horizontal lines for confidence intervals
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low, xmax = conf.high), 
                            height = 0.2, linewidth = 1.2) +
    # Add points for the estimate
    ggplot2::geom_point(size = 3) +
    # Add a vertical reference line at 0
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "blue", linewidth = 2) +
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