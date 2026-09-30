#' @title Perform a One-Proportion Test and Generate an HTML Table
#' @description This function performs a one-proportion test, calculating a confidence interval
#'   using the `binom` package and a p-value using the `stats` package. The results are
#'   formatted into a styled HTML table using `kableExtra`, utilizing `data.table` for data processing.
#'
#' @param data A data frame or data.table containing the variables for the test.
#' @param variable A variable from the `data` frame to use as the response. Must be unquoted or a string.
#' @param conf_normal A numeric value specifying the confidence level for the interval (e.g., 0.95).
#' @param alt_normal A character string specifying the alternative hypothesis direction.
#'   Can be "two.sided", "greater", or "less".
#' @param nh_normal A numeric value for the null hypothesis proportion (p0).
#' @param nd_num An integer specifying the number of decimal places for the output.
#' @param font_size An integer to set the font size of the table.
#' @param miss_yn A logical value; if `TRUE`, missing values are included in the total 'N' count.
#'   If `FALSE`, they are excluded.
#' @param meth A character string specifying the confidence interval method to be used by
#'   `binom::binom.confint()`. Options include "exact", "ac", "asymptotic", and "wilson".
#' @param testyn_normal A logical value; if `TRUE`, a p-value column is included in the output table.
#'
#' @return A `list` containing one element: `table`, which is a `kableExtra` HTML table object
#'   that summarizes the results of the one-proportion test.
#'
#' @examples
#' # Load necessary libraries
#' library(data.table)
#' library(knitr)
#' library(kableExtra)
#' library(gtsummary)
#' 
#' # Example 1: Basic usage with default settings on gtsummary's trial dataset
#' trial <- as.data.table(trial)
#' trial$response <- factor(trial$response, levels = c("1","0"))
#' rcs_oneprop_normal(data = trial,
#'                    variable = "response",
#'                    conf_normal = 0.95,
#'                    alt_normal = "two.sided",
#'                    nh_normal = 0.5,
#'                    nd_num = 3,
#'                    font_size = 16,
#'                    meth = "asymptotic")
#'
#' # Example 2: Exclude p-value and change CI method
#' rcs_oneprop_normal(data = trial,
#'                    variable = "response",
#'                    conf_normal = 0.95,
#'                    alt_normal = "two.sided",
#'                    nh_normal = 0.5,
#'                    nd_num = 3,
#'                    font_size = 12,
#'                    testyn_normal = FALSE,
#'                    meth = "exact")
#'
#' # Example 3: Different null hypothesis and alternative direction
#' rcs_oneprop_normal(data = trial,
#'                    variable = "response",
#'                    conf_normal = 0.95,
#'                    alt_normal = "greater",
#'                    nh_normal = 0.6,
#'                    nd_num = 3,
#'                    font_size = 12,
#'                    miss_yn = FALSE,
#'                    testyn_normal = TRUE,
#'                    meth = "exact")
#'
#' rcs_oneprop_normal(data = trial,
#'                    variable = "response",
#'                    conf_normal = 0.95,
#'                    alt_normal = "greater",
#'                    nh_normal = 0.6,
#'                    nd_num = 3,
#'                    font_size = 12,
#'                    miss_yn = FALSE,
#'                    testyn_normal = TRUE,
#'                    meth = "asymptotic")
#'
rcs_oneprop_normal <- function(data, variable, conf_normal, alt_normal, nh_normal,
                               nd_num, font_size, miss_yn = FALSE, meth, testyn_normal = FALSE,
                               miss_text = "Mis") {
  
  # Ensure data is handled as a data.table
  dt <- data.table::as.data.table(data)
  
  # --- 1. PREPARE DATA AND RUN TEST ---
  var_data <- factor(dt[[variable]])
  levelchosen <- levels(var_data)[1]
  
  xx <- sum(var_data == levelchosen, na.rm = TRUE)
  nn <- length(var_data)
  mm <- sum(is.na(var_data))
  valid_n <- nn - mm
  
  fit <- binom_core(xx, valid_n, conf.level = conf_normal, methods = meth)
  
  ssee <- NA_character_
  if (meth == "asymptotic") {
    df_val <- valid_n - 1
    t_score <- qt(1 - (1 - conf_normal) / 2, df_val)
    sval <- (fit$upper - fit$lower) / (2 * t_score)
    ssee <- paste0(ndformat(sval * 100, nd_num), "%")
  }
  
  ci <- paste0("(", ndformat(fit$lower * 100, nd_num), "%, ", ndformat(fit$upper * 100, nd_num), "%)")
  number <- ndformat(xx / valid_n * 100, nd_num)
  sp <- paste0(number, "%")
  
  test_fit <- stats::binom.test(xx, valid_n, conf.level = conf_normal, alternative = alt_normal, p = nh_normal)
  pvalue <- pvformat(test_fit$p.value)
  
  # Correct N if missing values are not included in the count
  if (!miss_yn) nn <- valid_n
  
  # --- 2. BUILD THE SUMMARY TABLE DATA AND HTML STRINGS ---
  
  # Dynamically build table columns and headers to avoid post-hoc filtering bugs
  dlist <- list(Variable = levelchosen, n = nn)
  col_headers <- c(variable, "N")
  
  if (miss_yn) {
    dlist$Mis <- mm
    col_headers <- c(col_headers, miss_text)
  }
  
  dlist$x <- xx
  dlist$Sp <- sp
  col_headers <- c(col_headers, "Event", "Sample p")
  
  if (meth == "asymptotic") {
    dlist$Se <- ssee
    col_headers <- c(col_headers, "SE Prop")
  }
  
  ci_header_name <- paste0(conf_normal * 100, "% CI for p<sup>1</sup>")
  dlist$Ci <- ci
  col_headers <- c(col_headers, ci_header_name)
  
  if (testyn_normal) {
    dlist$Pval <- pvalue
    col_headers <- c(col_headers, "P-value<sup>2</sup>")
  }
  
  dframe <- data.table::as.data.table(dlist)
  
  # Create the HTML string for the table caption
  caption_html <- paste0(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Test for One Proportion</p>"
  )
  
  # Define method text using data.table::fcase
  metex <- data.table::fcase(
    meth == "exact", "Exact",
    meth == "ac", "Agresti-Coull",
    meth == "asymptotic", "Wald",
    meth == "wilson", "Wilson",
    default = meth
  )
  
  fn1 <- paste0("<i>Interval method: ", metex, "</i>")
  
  if (testyn_normal) {
    fn2_text <- data.table::fcase(
      alt_normal == "greater", paste0("Exact binomial test; H<sub>1</sub>:p&gt; ", nh_normal),
      alt_normal == "less", paste0("Exact binomial test; H<sub>1</sub>:p&lt; ", nh_normal),
      alt_normal == "two.sided", paste0("Exact binomial test; H<sub>1</sub>:p&ne; ", nh_normal)
    )
    fn2 <- paste0("<i>", fn2_text, "</i>")
    footnotes_html <- c(fn1, fn2)
  } else {
    footnotes_html <- fn1
  }
  
  # --- 3. BUILD THE KABLEEXTRA TABLE ---
  
  table_out <- knitr::kable(
    as.data.frame(dframe),
    format = "html",
    align = "c",
    col.names = col_headers,
    caption = caption_html,
    escape = FALSE
  ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    ) |>
    kableExtra::column_spec(
      column = 1:ncol(dframe),
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::column_spec(
      column = 1,
      bold = TRUE
    ) |>
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::row_spec(
      row = 1,
      extra_css = "border-bottom: 1px solid #666; border-top: 1px solid #ddd;"
    ) |>
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  list(table = table_out)
}