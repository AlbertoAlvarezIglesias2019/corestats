#' @title Perform a One-Sample t-Test
#' @description This function conducts a one-sample t-test for a single mean. It
#'   calculates key statistics, including the mean, standard deviation, and standard
#'   error, and presents the results along with a confidence interval and optional
#'   p-value in a customizable HTML table.
#'
#' @param data A data frame containing the variable to be analyzed.
#' @param variable A character string specifying the name of the numeric variable.
#' @param conf_ttest A numeric value between 0 and 1 specifying the confidence
#'   level for the confidence interval.
#' @param nh_ttest The numeric value of the null hypothesis mean (\eqn{\mu_0}).
#' @param alt_ttest A character string specifying the alternative hypothesis
#'   direction. Must be one of `"greater"`, `"less"`, or `"two.sided"`.
#' @param nd_num The number of decimal places for rounding the numeric output.
#' @param font_size The font size for the output HTML table.
#' @param miss_yn A logical value. If \code{TRUE}, a column for the number of
#'   missing values is included in the table. Defaults to \code{FALSE}.
#' @param testyn_ttest A logical value. If \code{TRUE}, a p-value for the
#'   hypothesis test is included in the table. Defaults to \code{FALSE}.
#' @return A list containing one element:
#'   \item{table}{A \code{kableExtra} HTML table object with the test results.}
#' @details The function uses the standard \code{t.test()} from base R to perform
#'   the statistical analysis. It conditionally includes columns for missing values
#'   and the p-value based on the \code{miss_yn} and \code{testyn_ttest} flags.
#' @seealso \code{\link[stats]{t.test}}
#' @importFrom stats t.test
#' @importFrom stats sd
#' @importFrom data.table as.data.table
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling column_spec row_spec footnote
#'
#' @examples
#' # Create a sample dataset from a normal distribution with some missing values
#' set.seed(789)
#' sample_data <- data.frame(
#'   my_var = c(rnorm(95, mean = 65, sd = 15), rep(NA, 5))
#' )
#'
#' # Example 1: Default usage with a two-sided test
#' # Includes missing value count and the p-value
#' rcs_onemean_ttest(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_ttest = 0.95,
#'   nh_ttest = 70,
#'   alt_ttest = "two.sided",
#'   nd_num = 2,
#'   font_size = 12,
#'   miss_yn = TRUE,
#'   testyn_ttest = TRUE
#' )
#'
#' # Example 2: No hypothesis test and no missing value count
#' rcs_onemean_ttest(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_ttest = 0.99,
#'   nh_ttest = 70,
#'   alt_ttest = "less",
#'   nd_num = 3,
#'   font_size = 14,
#'   miss_yn = FALSE,
#'   testyn_ttest = FALSE
#' )

rcs_onemean_ttest <- function(data, variable, conf_ttest, nh_ttest, alt_ttest, nd_num, font_size, miss_yn = FALSE, testyn_ttest = FALSE) {
  
  # --- 1. PREPARE DATA AND RUN T-TEST ---
  
  # Convert to data.table and extract the variable efficiently
  dt <- data.table::as.data.table(data)
  x_val <- dt[[variable]]
  
  # Run the one-sample t-test.
  fit <- stats::t.test(x_val, mu = nh_ttest, conf.level = conf_ttest, alternative = alt_ttest)
  
  # Calculate and format descriptive statistics.
  me <- ndformat(mean(x_val, na.rm = TRUE), nd_num)
  sd_val <- ndformat(stats::sd(x_val, na.rm = TRUE), nd_num)
  esti <- paste0(me, " (", sd_val, ")")
  se <- ndformat(fit$stderr, nd_num + 1)
  
  # Calculate and format the confidence interval.
  out_ci <- ndformat(fit$conf.int, nd_num)
  ci <- paste0("(", paste0(out_ci, collapse = ", "), ")")
  
  defr <- fit$parameter
  tt <- ndformat(fit$statistic, 2)
  pval <- pvformat(fit$p.value)
  
  # Get counts for N and missing values.
  nn <- length(x_val)
  mm <- sum(is.na(x_val))
  
  # Correct N if missing values are not to be included in the count.
  if (!miss_yn) nn <- sum(!is.na(x_val))
  
  # --- 2. BUILD THE TABLE DATA AND HTML STRINGS ---
  
  # Create the data frame for the kableExtra table.
  dframe <- data.frame(
    Variable = variable,
    N = nn,
    Mis = mm,
    Mean = esti,
    Se = se,
    Ci = ci,
    Tt = tt,
    Df = defr,
    Pval = pval,
    stringsAsFactors = FALSE
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c(
    "Variable", "N", "Mis", "Mean (SD)", "SE Mean",
    paste0(conf_ttest * 100, "% CI for &mu;<sup>1</sup>"),
    "T-Value", "DF",
    "P-value<sup>2</sup>"
  )
  
  # Conditionally remove columns for missing values and p-value.
  if (!miss_yn) {
    dframe$Mis <- NULL
    col_headers_html <- col_headers_html[col_headers_html != "Mis"]
  }
  if (!testyn_ttest) {
    dframe$Pval <- NULL
    dframe$Tt <- NULL
    dframe$Df <- NULL
    col_headers_html <- col_headers_html[!col_headers_html %in% c("T-Value", "DF", "P-value<sup>2</sup>")]
  }
  
  # Create the HTML string for the table caption.
  caption_html <- paste0(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>One-sample t-test</p>"
  )
  
  # Define the HTML strings for footnotes based on test type.
  fn1 <- if (alt_ttest == "two.sided") "Two-sided" else "One-sided"
  fn1 <- paste0("<i>", fn1, "<i>")
  
  if (testyn_ttest) {
    fn2 <- switch(alt_ttest,
                  "greater" = paste0("H<sub>1</sub>:&#956;&gt; &#956;<sub>0</sub>; &#956;<sub>0</sub>= ", nh_ttest),
                  "less" = paste0("H<sub>1</sub>:&#956;&lt;&#956;<sub>0</sub>; &#956;<sub>0</sub>= ", nh_ttest),
                  "two.sided" = paste0("H<sub>1</sub>:&#956;&ne;&#956;<sub>0</sub>; &#956;<sub>0</sub>= ", nh_ttest)
    )
    fn2 <- paste0("<i>", fn2, "<i>")
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
      column = 1:ncol(dframe),
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