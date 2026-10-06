#' @title Perform a Two-Sample t-Test
#' @description This function conducts a two-sample t-test to compare the means of a
#'   numeric variable across two groups defined by a factor. It generates two separate
#'   HTML tables: one for group-wise summary statistics and another for the t-test
#'   inference results.
#'
#' @param data A data frame or data.table containing the numeric and grouping variables.
#' @param variable A character string specifying the name of the numeric variable.
#' @param by A character string specifying the name of the grouping variable. This
#'   variable must be a factor with exactly two levels.
#' @param conf_ttest A numeric value between 0 and 1, specifying the confidence
#'   level for the confidence interval.
#' @param alt_ttest A character string specifying the alternative hypothesis
#'   direction. Must be one of `"greater"`, `"less"`, or `"two.sided"`.
#' @param ev A logical value. If \code{TRUE} (the default), a standard t-test assuming
#'   equal variances is performed. If \code{FALSE}, Welch's t-test is used.
#' @param nh_ttest A numeric value for the null hypothesis mean difference. Defaults
#'   to 0.
#' @param nd_num The number of decimal places for rounding the numeric output.
#' @param font_size The font size for the output HTML tables.
#' @param miss_yn A logical value. If \code{TRUE}, a column for the number of
#'   missing values is included in the summary table. Defaults to \code{FALSE}.
#' @param testyn_ttest A logical value. If \code{TRUE}, the p-value is included
#'   in the inference table. Defaults to \code{FALSE}.
#' @return A list containing two `kableExtra` HTML table objects:
#'   \item{table_summ}{A table of group-wise summary statistics (N, mean, SD).}
#'   \item{table_infe}{A table of t-test inference results (difference, SE, CI, p-value).}
#' @details The function performs a two-sample t-test using \code{stats::t.test()} and
#'   \code{data.table} to calculate group-wise statistics. It produces two distinct tables
#'   to separate the descriptive summary from the inferential results.
#' @note The grouping variable specified by `by` must be a factor with two levels.
#' @seealso \code{\link[stats]{t.test}}
#' @import data.table
#' @importFrom stats t.test sd
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling add_header_above column_spec row_spec footnote
#'
#' @examples
#' # Create a sample dataset for a two-sample t-test
#' set.seed(123)
#' sample_data <- data.frame(
#'   group = factor(rep(c("Group A", "Group B"), 50)),
#'   value = c(rnorm(50, mean = 20, sd = 3), rnorm(50, mean = 22, sd = 3))
#' )
#'
#' # Example 1: Basic two-sample t-test with a two-sided alternative
#' # Includes missing value count and p-value by default
#' rcs_twomean_ttest(
#'   data = sample_data,
#'   variable = "value",
#'   by = "group",
#'   conf_ttest = 0.95,
#'   alt_ttest = "two.sided",
#'   ev = TRUE,
#'   nd_num = 2,
#'   font_size = 12,
#'   miss_yn = TRUE,
#'   testyn_ttest = TRUE
#' )
#'
#' # Example 2: One-sided t-test assuming non-equal variances, no missing/p-value
#' rcs_twomean_ttest(
#'   data = sample_data,
#'   variable = "value",
#'   by = "group",
#'   conf_ttest = 0.99,
#'   alt_ttest = "less",
#'   ev = FALSE,
#'   nd_num = 3,
#'   font_size = 14,
#'   miss_yn = FALSE,
#'   testyn_ttest = TRUE
#' )

rcs_twomean_ttest <- function(data, variable, by, conf_ttest, alt_ttest, ev = TRUE, nh_ttest = 0,
                              nd_num, font_size, miss_yn = FALSE, testyn_ttest = FALSE,
                              miss_text = "Mis") {
  
  # Convert to data.table if not already
  dt <- data.table::as.data.table(data)
  
  var_data <- dt[[variable]]
  by_data <- factor(dt[[by]])
  levs <- levels(by_data)
  
  # --- 1. STATISTICS & INFERENCE ---
  
  # Run the two-sample t-test.
  fit <- stats::t.test(var_data ~ by_data, var.equal = ev, mu = nh_ttest, conf.level = conf_ttest, alternative = alt_ttest)
  
  # Calculate group-wise descriptive statistics using data.table
  dt_summ <- dt[, .(
    nn_total = .N,
    nn_valid = sum(!is.na(get(variable))),
    mm = sum(is.na(get(variable))),
    mean_val = mean(get(variable), na.rm = TRUE),
    sd_val = stats::sd(get(variable), na.rm = TRUE)
  ), by = .(group_var = get(by))]
  
  # Ensure the order strictly matches the factor levels of `by`
  dt_summ <- dt_summ[match(levs, group_var)]
  
  # Format Descriptive Statistics
  esti <- paste0(round(dt_summ$mean_val, nd_num), " (", round(dt_summ$sd_val, nd_num), ")")
  
  # Correct N if missing values are not to be included in the count.
  nn <- if (miss_yn) dt_summ$nn_total else dt_summ$nn_valid
  mm <- dt_summ$mm
  
  # Calculate and format the confidence interval.
  # Note: Assumes `ndformat` and `pvformat` are sourced in your environment as per original code.
  diff <- ndformat(fit$estimate[1] - fit$estimate[2], nd_num)
  se <- ndformat(fit$stderr, nd_num + 1)
  out <- ndformat(fit$conf.int, nd_num)
  out <- paste(out, collapse = ", ")
  ci <- paste0("(", out, ")")
  defr <- fit$parameter
  tt <- round(fit$statistic, 2)
  
  # Format the p-value with boundary conditions.
  pval <- pvformat(fit$p.value)
  
  # --- 2. BUILD THE SUMMARY TABLE DATA AND HTML STRINGS ---
  
  dframe <- data.frame(
    Variable = variable,
    N1 = nn[1],
    Mis1 = mm[1],
    Mean1 = esti[1],
    N2 = nn[2],
    Mis2 = mm[2],
    Mean2 = esti[2],
    stringsAsFactors = FALSE
  )
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c("Variable", "N", miss_text, "Mean (SD)", "N", miss_text, "Mean (SD)")
  header_vector <- stats::setNames(c(1, 3, 3), c(" ", levs[1], levs[2]))
  
  # Conditionally remove columns for missing values.
  if (!miss_yn) {
    dframe$Mis1 <- NULL
    dframe$Mis2 <- NULL
    col_headers_html <- col_headers_html[!col_headers_html %in% miss_text]
    header_vector <- stats::setNames(c(1, 2, 2), c(" ", levs[1], levs[2]))
  }
  
  caption_html <- paste0(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Descriptive Statistics</p>"
  )
  
  # --- 3. BUILD THE KABLEEXTRA SUMMARY TABLE ---
  
  aa <- if (miss_yn) 1 else 0
  
  table_out_summ <- knitr::kable(
    dframe,
    format = "html",
    align = "c",
    col.names = col_headers_html,
    caption = caption_html,
    escape = FALSE
  )  |> 
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    )  |>
    kableExtra::add_header_above(header_vector) |>
    kableExtra::column_spec(
      column = 1:ncol(dframe),
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::column_spec(
      column = c(1, 3 + aa, 5 + 2 * aa), 
      border_right = "3px solid #666" 
    ) |>
    kableExtra::column_spec(
      column = 1, 
      border_left = "3px solid #666",
      bold = TRUE
    )  |>
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd;; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::row_spec(
      row = 1,
      extra_css = "border-bottom: 2px solid #666; border-top: 1px solid #ddd;"
    ) 
  
  # --- 4. BUILD THE INFERENCE DATA AND HTML STRINGS ---
  
  dframe_infe <- data.frame(
    diff = diff,
    Se = se,
    Ci = ci,
    Tt = tt,
    Df = defr,
    Pval = pval,
    stringsAsFactors = FALSE
  ) 
  row.names(dframe_infe) <- NULL
  
  col_headers_infe <- c(
    "Diff in means", "SE Diff",
    paste0(conf_ttest * 100, "% CI for &mu;<sub>1</sub> - &mu;<sub>2</sub><sup>1</sup>"),
    "T-Value", "DF",
    "P-value<sup>2</sup>"
  )
  
  # Conditionally remove inference parameters
  if (!testyn_ttest) {
    dframe_infe$Pval <- NULL
    dframe_infe$Tt <- NULL
    dframe_infe$Df <- NULL
    col_headers_infe <- col_headers_infe[!col_headers_infe %in% c("T-Value", "DF", "P-value<sup>2</sup>")]
  }
  
  caption_html_infe <- paste0(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Two-sample t-test</p>"
  )
  
  # Define the HTML strings for footnotes based on test type without case_when/if_else
  fn_equal_var <- if (ev) "<i>Note: Equal variances assumed<i>" else "<i>Note: Equal variances are not assumed<i>"
  
  if (testyn_ttest) {
    fn1 <- if (alt_ttest == "two.sided") "Two-sided" else "One-sided"
    fn2 <- if (alt_ttest == "greater") {
      paste0("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub>&gt;", nh_ttest)
    } else if (alt_ttest == "less") {
      paste0("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub>&lt;", nh_ttest)
    } else {
      paste0("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub>&ne;", nh_ttest)
    }
    
    footnotes_html <- c(paste0("<i>", fn1, "<i>"), paste0("<i>", fn2, "<i>"))
  } else {
    fn1 <- if (alt_ttest == "two.sided") "Two-sided" else "One-sided"
    footnotes_html <- paste0("<i>", fn1, "<i>")
  }
  
  # --- 5. BUILD THE KABLEEXTRA INFERENCE TABLE ---
  
  table_out_infe <- knitr::kable(
    dframe_infe,
    format = "html",
    align = "c", # Horizontal center
    col.names = col_headers_infe,
    caption = caption_html_infe,
    escape = FALSE
  ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    ) |>
    kableExtra::column_spec(
      column = 1:ncol(dframe_infe),
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      # Added vertical-align: middle; here
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px; vertical-align: middle;"
    ) |>
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      # Added vertical-align: middle; here
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd; padding-left: 10px; padding-right: 10px; vertical-align: middle;"
    ) |>
    kableExtra::row_spec(
      row = 1,
      # Added vertical-align: middle; here
      extra_css = "border-bottom: 2px solid #666; border-top: 1px solid #ddd; vertical-align: middle;"
    ) |>
    kableExtra::footnote(
      general = fn_equal_var,
      number = footnotes_html,
      general_title = "",
      escape = FALSE
    )
  
  
  # Return the table as a list.
  list(table_summ = table_out_summ, table_infe = table_out_infe)
}

