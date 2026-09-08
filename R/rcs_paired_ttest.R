#' @title Perform a Paired T-Test and Generate an HTML Table
#' @description This function performs a paired t-test on two variables from a data frame,
#'   calculates a confidence interval, and formats the results into a styled HTML
#'   table using the `kableExtra` package.
#'
#' @param var1 A numeric vector representing the first variable.
#' @param var2 A numeric vector representing the second variable.
#' @param conf_ttest A numeric value specifying the confidence level for the interval (e.g., 0.95).
#' @param nh_ttest A numeric value for the null hypothesis mean difference (mu0).
#' @param alt_ttest A character string specifying the alternative hypothesis direction.
#'   Can be "two.sided", "greater", or "less".
#' @param nd_num An integer specifying the number of decimal places for the output.
#' @param font_size An integer to set the font size of the table.
#' @param miss_yn A logical value; if `TRUE`, missing values are included in the total 'N' count.
#'   If `FALSE`, they are excluded.
#' @param testyn_ttest A logical value; if `TRUE`, columns for T-Value, DF, and P-value are included in the output table.
#'
#' @return A `list` containing one element: `table`, which is a `kableExtra` HTML table object
#'   that summarizes the results of the paired t-test.
#'
#' @examples
#' # Create example data
#' data_paired <- data.frame(
#'   before = c(10, 12, 15, 14, 18),
#'   after = c(15, 13, 16, 17, 20)
#' )
#'
#' # Example 1: Basic usage with a two-sided test
#' rcs_paired_ttest(
#'   var1 = data_paired$before,
#'   var2 = data_paired$after,
#'   conf_ttest = 0.95,
#'   nh_ttest = 0,
#'   alt_ttest = "two.sided",
#'   nd_num = 3,
#'   font_size = 14,
#'   testyn_ttest = TRUE
#' )
#'
#' # Example 2: Exclude the test statistics and p-value
#' # This shows how to calculate only the confidence interval for the mean difference.
#' rcs_paired_ttest(
#'   var1 = data_paired$before,
#'   var2 = data_paired$after,
#'   conf_ttest = 0.95,
#'   nh_ttest = 0,
#'   alt_ttest = "two.sided",
#'   nd_num = 3,
#'   font_size = 14,
#'   testyn_ttest = FALSE
#' )
#'
#' # Example 3: Different confidence level and one-sided test
#' # This example uses a one-sided test with a different confidence level.
#' rcs_paired_ttest(
#'   var1 = data_paired$before,
#'   var2 = data_paired$after,
#'   conf_ttest = 0.90,
#'   nh_ttest = 0,
#'   alt_ttest = "less",
#'   nd_num = 3,
#'   font_size = 14
#' )
#' 
#' 

rcs_paired_ttest <- function(var1,var2, conf_ttest, nh_ttest, alt_ttest, nd_num, font_size, miss_yn = FALSE, testyn_ttest = FALSE) {
  
  
  # --- 1. PREPARE DATA AND RUN T-TEST ---
  # Run the one-sample t-test.
  fit <- stats::t.test(var1,var2,paired=TRUE, mu = nh_ttest, conf.level = conf_ttest, alternative = alt_ttest)
  
  # Calculate and format descriptive statistics.
  esti <- ndformat(fit$estimate,nd) 
  se <- ndformat(fit$stderr, nd + 1)
  
  # Calculate and format the confidence interval.
  out <- ndformat(fit$conf.int,nd) 
  #out <- round(fit$conf.int, nd)
  out <- paste(out, collapse = ", ")
  ci <- paste("(", out, ")", sep = "")
  
  defr <- fit$parameter
  tt <- round(fit$statistic,2)
  # Format the p-value with boundary conditions.
  pval <- pvformat(fit$p.value)
  
  
  # Get counts for N and missing values.
  nn <- length(var1)
  mm <- nn - dim(cbind(var1,var2)  |>  na.omit())[1]
  
  
  # Correct N if missing values are not to be included in the count.
  if (!miss_yn) nn <- nn-mm
  
  # --- 2. BUILD THE DATA.TABLE AND HTML STRINGS ---
  dframe <- data.table::data.table(
    N    = nn,
    Mis  = mm,
    Mean = esti,
    Se   = se,
    Ci   = ci,
    Tt   = tt,
    Df   = defr,
    Pval = pval
  )
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c("N", "Mis", "Mean Diff", "SE Mean Diff",
                        paste0(conf_ttest * 100, "% CI for d<sup>1</sup>"),
                        "T-Value","DF",
                        "P-value<sup>2</sup>"
  )
  
  # Conditionally remove columns for missing values and p-value.
  if (!miss_yn) {
    dframe[, Mis := NULL]
    col_headers_html <- col_headers_html[!col_headers_html %in% "Mis"]
  }
  if (!testyn_ttest) {
    dframe[, c("Pval", "Tt", "Df") := NULL]
    col_headers_html <- col_headers_html[!col_headers_html %in% c("T-Value","DF","P-value<sup>2</sup>")]
  }
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Paired t-test</p>",
    sep = ""
  )
  
  # Define the HTML strings for footnotes based on test type.
  fn1_text <- data.table::fifelse(alt_ttest == "two.sided", "Two-sided", "One-sided")
  fn1 <- paste0("<i>", fn1_text, "</i>")
  
  if (testyn_ttest) {
    fn2_body <- data.table::fcase(
      alt_ttest == "greater",   paste0("H<sub>1</sub>:d&gt; ", nh_ttest),
      alt_ttest == "less",      paste0("H<sub>1</sub>:d&lt; ", nh_ttest),
      alt_ttest == "two.sided", paste0("H<sub>1</sub>:d&ne; ", nh_ttest)
    )
    fn2 <- paste0("<i>", fn2_body, "</i>")
    footnotes_html <- c(fn1, fn2)
  } else {
    footnotes_html <- fn1
  }
  
  
  
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
    # Add vertical borders and padding to columns.
    kableExtra::column_spec(
      column = 1:dim(dframe)[2],
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    )  |> 
    # Make the first column (Variable) bold.
    kableExtra::column_spec(
      column = 1,
      bold = TRUE
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
  
  # Return the table as a list.
  list(table = table_out)
}