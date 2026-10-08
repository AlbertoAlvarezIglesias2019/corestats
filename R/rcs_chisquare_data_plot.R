#' @title Generate a Mosaic Plot for Two Categorical Variables
#'
#' @description This function creates a mosaic plot to visualize the relationship
#' between two categorical variables. A mosaic plot is a graphical representation
#' of a two-way contingency table, where the tile areas correspond to the cell counts.
#'
#' @param data A data frame containing the two categorical variables.
#' @param variable A character string specifying the name of the first categorical variable.
#' @param by A character string specifying the name of the second categorical variable.
#'
#' @return (No return value, a plot is drawn to the active graphics device.)
#'
#' @details
#' The function first creates a contingency table from the specified `variable` and `by`
#' columns. It then uses base R's `plot()` function on the transposed table to
#' generate the mosaic plot. This plot is useful for visually inspecting the
#' frequencies and conditional proportions of the two variables, which is a key
#' step in chi-square analysis.
#'
#' @seealso \code{\link[base]{plot}}, \code{\link[stats]{chisq.test}}
#'
#' @examples
#' # Create a sample data frame
#' data_df <- data.frame(
#'   gender = factor(c(rep("Male", 10), rep("Female", 40))),
#'   opinion = factor(c(rep("Agree", 5), rep("Disagree", 25), rep("Agree", 10), rep("Disagree", 10)))
#' )
#'
#' # Generate a mosaic plot of gender by opinion
#' rcs_chisquare_data_plot(
#'   data = data_df,
#'   variable = "gender",
#'   by = "opinion"
#' )
#' 



rcs_chisquare_data_plot <- function(data, rrr, ccc,rrr_lab,ccc_lab) {
  
  
  #var_data <- data[[variable]]
  #by_data <- factor(data[[by]])
  
  
  #ttt <- table(var_data,by_data)
  #plot(t(ttt),xlab=by,ylab= variable,main = "Mosaic Plot")  
  par(cex.lab = 1.5)
  
  mosaicplot(table(factor(data[[ccc]]), data[[rrr]]), 
             main = "Mosaic Plot of Association", 
             xlab=ccc_lab,ylab= rrr_lab,
             color = c("#2ECC71", "#E74C3C"),
             las = 1,
             cex.axis = 1.3)
  
  

}