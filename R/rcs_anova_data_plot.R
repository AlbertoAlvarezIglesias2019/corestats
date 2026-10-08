#' @title Generate a Simple Boxplot of Data
#'
#' @description This function creates a basic boxplot to visualize the distribution
#' of a continuous variable across different groups of a categorical variable. It is
#' intended for initial data exploration and visualization.
#'
#' @param data A data frame containing the variables.
#' @param variable A character string specifying the name of the dependent variable (the continuous variable).
#' @param by A character string specifying the name of the grouping variable (the categorical variable).
#'
#' @return A `ggplot` object representing the boxplot.
#'
#' @details
#' This function provides a fundamental visualization of the raw data before any
#' statistical tests are applied. It uses `ggplot2::geom_boxplot()` to display
#' the median, quartiles, and potential outliers for each group.
#'
#' @seealso \code{\link{anova____ph_inference_plot}}, \code{\link{anova____ph_interval_plot}}
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
#' # Generate a boxplot of the data
#' rcs_anova_data_plot(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose"
#' )
#' 
#' 

rcs_anova_data_plot <- function(data, variable, by) {
  
  
  formu <- as.formula(paste(variable ,"~", by,sep=""))
  
  ########################################
  ## Creates the boxplot + inference plot
  ########################################
  # 2. Build the plot step-by-step
  data_plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = factor(.data[[by]]), y = .data[[variable]])) +
    # A. Add the boxplot
    ggplot2::geom_boxplot() +
    
    # D. Add titles and labels
    ggplot2::labs(
      title = NULL,
      x = by,
      y = variable
    ) +
    # F. (Optional) Customize the theme for a cleaner look
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 17),
      axis.text = ggplot2::element_text(size = 19),
      legend.title = ggplot2::element_text(size = 15),
      legend.text = ggplot2::element_text(size = 13)
    )
  
  data_plot
}