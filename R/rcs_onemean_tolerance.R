#' @title Calculate and Format a Normal Tolerance Interval
#' @description This function calculates a tolerance interval for a single variable
#'   assumed to be from a normal distribution. It returns the result in a styled
#'   HTML table and provides the necessary parameters to generate a corresponding
#'   plot.
#'
#' @param data A data frame containing the variable to be analyzed.
#' @param variable A character string specifying the name of the numeric variable
#'   in the data frame.
#' @param conf_tolerance A numeric value between 0 and 1, representing the confidence
#'   level of the interval.
#' @param poco_tolerance A numeric value between 0 and 1, representing the
#'   proportion of the population to be covered by the interval.
#' @param side_tolerance A numeric value, either 1 for a one-sided interval or
#'   2 for a two-sided interval.
#' @param plotype_tolerance A character string specifying the type of plot to
#'   generate (e.g., "qq", "pp", "histogram", "boxplot").
#' @param nd_num The number of decimal places to round the interval to.
#' @param font_size The font size for the output HTML table.
#' @return A list containing two elements:
#'   \item{table}{A \code{kableExtra} HTML table object showing the tolerance interval.}
#'   \item{plot}{A list of parameters for a plot, typically for a Jamovi backend to render.}
#' @details The function relies on the \code{tolerance} package to calculate the interval
#'   based on a normally distributed sample. It handles missing values by removing them
#'   before the calculation.
#' @note This function is designed to be part of a larger analysis, likely a
#'   Jamovi module, and does not produce a standalone plot. Instead, it passes
#'   plot parameters for external rendering.
#' @seealso \code{\link[tolerance]{normtol.int}}, \code{\link[tolerance]{plottol}},
#'   \code{\link[kableExtra]{kableExtra}}
#' @importFrom tolerance normtol.int
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling column_spec row_spec footnote
#'
#' @examples
#' # Create a sample dataset from a normal distribution
#' set.seed(456)
#' sample_data <- tibble::tibble(
#'   my_var = rnorm(100, mean = 25, sd = 5)
#' )
#'
#' # Example 1: Basic two-sided tolerance interval
#' rcs_onemean_tolerance(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_tolerance = 0.95,
#'   poco_tolerance = 0.99,
#'   side_tolerance = 2,
#'   plotype_tolerance = "both",
#'   nd_num = 2,
#'   font_size = 12
#' )$table
#'
#' # Example 2: One-sided interval with different confidence and proportion
#' rcs_onemean_tolerance(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_tolerance = 0.99,
#'   poco_tolerance = 0.95,
#'   side_tolerance = 1,
#'   plotype_tolerance = "hist",
#'   nd_num = 3,
#'   font_size = 14
#' )$table
#' 
#' # Example 3: One-sided interval with different confidence and proportion (plot)
#' tp <- rcs_onemean_tolerance(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_tolerance = 0.99,
#'   poco_tolerance = 0.95,
#'   side_tolerance = 1,
#'   plotype_tolerance = "both",
#'   nd_num = 3,
#'   font_size = 14
#' )$plot
#' 
#' tolerance::plottol(tol.out=tp$tol.out,x=tp$x,plot.type=tp$plot.type,y.lab=tp$y.lab)
#'           


rcs_onemean_tolerance <- function(data, variable, conf_tolerance, poco_tolerance, side_tolerance, plotype_tolerance, nd_num, font_size) {
  
  # --- 1. PREPARE DATA AND CALCULATE TOLERANCE INTERVAL ---
  

  # Extract the variable, ensuring a clean vector without NAs.
  #var_data <- data[[variable]]
  #var_data <- var_data[!is.na(var_data)]
  
  # Calculate the tolerance interval using the 'tolerance' package.
  # Note: `alpha` is 1 - confidence level.
  #fit <- tolerance::normtol.int(
  #  x = var_data,
  #  alpha = 1 - conf_tolerance,
  #  P = poco_tolerance,
  #  side = as.numeric(side_tolerance)
  #)
  
  # --- 1. PREPARE DATA AND CALCULATE TOLERANCE INTERVAL ---
  
  # Convert to data.table and clean NAs
  dt <- data.table::as.data.table(data)
  var_data <- dt[[variable]]
  var_data <- var_data[!is.na(var_data)]
  
  n <- length(var_data)
  if (n < 2) {
    stop("Insufficient non-missing data points to calculate tolerance intervals.")
  }
  
  x_bar <- mean(var_data)
  s_val <- stats::sd(var_data)
  nu <- n - 1
  alpha <- 1 - conf_tolerance
  side <- as.numeric(side_tolerance)
  
  # Calculate k-factor
  if (side == 2) {
    # Howe's Method for 2-sided Normal Tolerance Factor
    z_p <- stats::qnorm((1 + poco_tolerance) / 2)
    chi2_alpha <- stats::qchisq(alpha, df = nu)
    
    k_factor <- z_p * sqrt((nu / chi2_alpha) * (1 + 1 / n) * (1 + (nu - chi2_alpha - 2) / (2 * (n + 1)^2)))
    
    lower_bound <- x_bar - k_factor * s_val
    upper_bound <- x_bar + k_factor * s_val
    
    fit <- data.frame(
      alpha = alpha,
      P = poco_tolerance,
      x.bar = x_bar,
      `2-sided.lower` = lower_bound,
      `2-sided.upper` = upper_bound,
      check.names = FALSE
    )
  } else {
    # Exact non-central t-distribution approach for 1-sided Normal Tolerance Factor
    z_p <- stats::qnorm(poco_tolerance)
    ncp_val <- z_p * sqrt(n)
    
    #k_factor <- stats::qt(p = conf_tolerance, df = nu, ncp = ncp_val) / sqrt(n)
    
    # Suppress internal C-routine precision warnings from qt()
    k_factor <- suppressWarnings(
      stats::qt(p = conf_tolerance, df = nu, ncp = ncp_val)
    ) / sqrt(n)
    
    
    lower_bound <- x_bar - k_factor * s_val
    upper_bound <- x_bar + k_factor * s_val
    
    fit <- data.frame(
      alpha = alpha,
      P = poco_tolerance,
      x.bar = x_bar,
      `1-sided.lower` = lower_bound,
      `1-sided.upper` = upper_bound,
      check.names = FALSE
    )
  }
  
  
  # Extract the interval bounds and format them into a single string.
  ttt1 <- ndformat(fit[, 4], nd_num)
  ttt2 <- ndformat(fit[, 5], nd_num)
  ti <- paste("(", ttt1, ",", ttt2, ")", sep = "")
  
  # --- 2. BUILD THE TABLE DATA AND HTML STRINGS ---
  
  # Create the data frame for the kableExtra table.
  dframe <- data.frame(Ci = ti)
  row.names(dframe) <- NULL
  
  # Create the table header string with HTML formatting.
  col_headers_html <- paste(conf_tolerance * 100, "% Tolerance Interval<sup>1</sup>", sep = "")
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Tolerance Interval</p>",
    sep = ""
  )
  
  # Create the HTML strings for footnotes based on the parameters.
  fn1 <- paste("Proportion Covered = ", poco_tolerance, sep = "")
  #fn2 <- dplyr::if_else(side_tolerance == 2, "2-sided", "1-sided")
  fn2 <- data.table::fifelse(side_tolerance == 2, "2-sided", "1-sided")
  fn <- paste(fn1, fn2, sep = "; ")
  fn <- paste("<i>", fn, "<i>", sep = "")
  footnotes_html <- fn
  
  # --- 3. BUILD THE KABLEEXTRA TABLE ---
  
  table_out <- knitr::kable(
    dframe,
    format = "html",
    align = "c",
    col.names = col_headers_html,
    caption = caption_html,
    escape = FALSE
  )  |> 
    # Style the table layout and font.
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    )  |> 
    # Add borders and spacing to columns.
    kableExtra::column_spec(
      column = 1:dim(dframe)[2],
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    )  |> 
    # Add horizontal borders to the header and bold the text.
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd;; padding-left: 10px; padding-right: 10px;"
    )  |> 
    # Add horizontal borders to the first data row.
    kableExtra::row_spec(
      row = 1,
      extra_css = "border-bottom: 2px solid #666; border-top: 1px solid #ddd;"
    )  |> 
    # Add footnotes to the table.
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  # --- 4. RETURN TABLE AND PLOT PARAMETERS ---
  
  # The plot is returned as a list of parameters for a Jamovi backend to render.
  list(table = table_out, plot = list(tol.out = fit, x = var_data, plot.type = plotype_tolerance, y.lab = variable,font_size=font_size))
}