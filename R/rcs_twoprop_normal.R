#' @title Two-sample Proportions Test
#'
#' @description
#' Performs a two-sample proportions test using the normal approximation (equivalent to a Chi-squared test) and formats the output into professional HTML tables.
#'
#' @param data A data frame or data.table containing the variables for the analysis.
#' @param variable The name of the outcome variable (as a string). This should be a factor or a variable with two levels, where the first level is considered the event of interest.
#' @param by The name of the grouping variable (as a string). This must be a factor with exactly two levels.
#' @param conf_normal The confidence level for the interval, as a decimal (e.g., `0.95` for a 95% confidence interval).
#' @param alt_normal A string specifying the alternative hypothesis, one of `"two.sided"`, `"less"`, or `"greater"`.
#' @param yatc A logical value (`TRUE`/`FALSE`) indicating whether to apply Yates' continuity correction.
#' @param nd_num The number of decimal places for rounding the output (formerly `nd`).
#' @param font_size The font size for the output tables (in pixels).
#' @param miss_yn A logical value (`TRUE`/`FALSE`). If `TRUE`, missing values are counted and displayed in the descriptive table; otherwise, they are omitted (formerly `imis`).
#' @param testyn_normal A logical value (`TRUE`/`FALSE`). If `TRUE`, the inference table with test statistics and P-value is included in the output.
#'
#' @details
#' This function uses `prop.test()` from base R to perform the statistical test. The data is prepared using `data.table` operations, subsetting based on the `by` variable, and handling missing values.
#'
#' The function produces two HTML tables formatted using `kableExtra`:
#' - **Descriptive Statistics Table**: Summarizes the sample sizes ($N$), missing values ($Mis$), and proportions ($\% (n)$) for each of the two groups.
#' - **Test for Two Proportions Table**: Provides the difference in proportions, confidence interval, $\chi^2$ statistic, degrees of freedom, and P-value.
#'
#' @return A list containing two `kableExtra` HTML table objects:
#'   - `table_summ`: An HTML object representing the descriptive statistics table.
#'   - `table_infe`: An HTML object representing the test for two proportions table.
#'
#' @seealso [prop.test()], [data.table::data.table()], [kableExtra::kable_styling()]
#'
#' @examples
#' # A simple, runnable example
#' library(data.table)
#' 
#' # Create a dummy data frame
#' dummy_data <- data.frame(
#'   outcome = sample(factor(c("Yes", "No", "Yes", "No", "Yes", "Yes", "No", "No", "Yes", "No")), size = 20, replace = TRUE),
#'   group = sample(factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B")), size = 20, replace = TRUE)
#' )
#'
#' # Run the function with sample parameters
#' results <- rcs_twoprop_normal(
#'   data = dummy_data,
#'   variable = "outcome",
#'   by = "group",
#'   conf_normal = 0.95,
#'   alt_normal = "two.sided",
#'   nh_normal = 0.1,
#'   yatc = TRUE,
#'   nd_num = 2,
#'   font_size = 14,
#'   miss_yn = FALSE,
#'   testyn_normal = TRUE
#' )
#'
#' # Access the returned tables
#' if (requireNamespace("knitr", quietly = TRUE) && requireNamespace("kableExtra", quietly = TRUE)) {
#'   print(results$table_summ)
#'   print(results$table_infe)
#' }
#' 
#' rcs_twoprop_normal(
#'   data = dummy_data,
#'   variable = "outcome",
#'   by = "group",
#'   conf_normal = 0.95,
#'   alt_normal = "two.sided",
#'   yatc = TRUE,
#'   nd_num = 2,
#'   font_size = 14,
#'   miss_yn = FALSE,
#'   meth = "asymptotic",
#'   testyn_normal = TRUE
#' ) 
#' 
#' rcs_twoprop_normal(
#'   data = dummy_data,
#'   variable = "outcome",
#'   by = "group",
#'   conf_normal = 0.95,
#'   alt_normal = "two.sided",
#'   yatc = TRUE,
#'   nd_num = 2,
#'   font_size = 14,
#'   miss_yn = FALSE,
#'   meth = "score",
#'   testyn_normal = TRUE
#' ) 
#' @export
rcs_twoprop_normal <- function(data, variable, by, conf_normal, alt_normal, yatc,
                               nd_num, font_size, miss_yn = FALSE, meth = "asymptotic", testyn_normal = FALSE,
                               miss_text = "Mis", nh_normal = 0
                               ) {
  
  # --- 1. PREPARE DATA AND RUN TEST USING data.table ---
  
  dt <- data.table::as.data.table(data)
  
  dt_work <- data.table::data.table(outcome = dt[[variable]],
                        group = dt[[by]])
  
  misgroup <- sum(is.na(dt_work$group))
  dt_work <- dt_work[!is.na(group)]
  
  if (!is.factor(dt_work$group)) dt_work[, group := as.factor(group)]
  if (!is.factor(dt_work$outcome)) dt_work[, outcome := as.factor(outcome)]
  
  group_levels <- levels(dt_work$group)
  grol <- group_levels[1]
  refl <- levels(dt_work$outcome)[1]
  
  # Missing outcome counts by group
  misoutcome <- dt_work[, .(m = sum(is.na(outcome))), by = .(group = factor(group, levels = group_levels))]
  
  # Summary of non-missing values by group
  summary_df <- dt_work[!is.na(outcome) & !is.na(group), .(
    n = .N,
    x = sum(outcome == refl)
  ), by = .(group = factor(group, levels = group_levels))]
  
  # Ensure all group levels are represented
  all_groups <- data.table::data.table(group = factor(group_levels, levels = group_levels))
  summary_df <- merge(all_groups, summary_df, by = "group", all.x = TRUE)
  summary_df[is.na(n), n := 0]
  summary_df[is.na(x), x := 0]
  
  summary_df <- merge(summary_df, misoutcome, by = "group", all.x = TRUE)
  summary_df[is.na(m), m := 0]
  
  x1 <- summary_df[group == group_levels[1], x]
  x2 <- summary_df[group == group_levels[2], x]
  n1 <- summary_df[group == group_levels[1], n]
  n2 <- summary_df[group == group_levels[2], n]
  
  # Calculate and format descriptive statistics.
  cc <- summary_df$x
  pp <- round(summary_df$x / summary_df$n * 100, nd_num)
  pp <- paste(pp, "%", sep = "")
  esti <- paste(pp, " (", cc, ")", sep = "")
  
  # Get counts for N and missing values.
  nn <- summary_df$n
  mm <- summary_df$m
  
  # Correct N if missing values are not to be included in the count.
  if (!miss_yn) nn <- nn - mm
  
  # Run the test
  fit <- prop.test(c(x1, x2), c(n1, n2), conf.level = conf_normal, alternative = alt_normal, correct = yatc)
  
  # Calculate and format the confidence interval.
  diff <- paste(ndformat((fit$estimate[1] - fit$estimate[2]) * 100, nd_num), "%", sep = "")
  out <- paste(ndformat(fit$conf.int * 100, nd_num), "%", sep = "")
  out <- paste(out, collapse = ", ")
  ci <- paste("(", out, ")", sep = "")
  
  if (meth == "asymptotic") {
    ffiitt <- wald_prop_diff_ci(x1, n1, x2, n2, conf.level = conf_normal, alternative = alt_normal, nh_normal = nh_normal)
    out <- paste(ndformat(c(ffiitt$Lower_CI, ffiitt$Upper_CI) * 100, nd_num), "%", sep = "")
    out <- paste(out, collapse = ", ")
    ci <- paste("(", out, ")", sep = "")
    ssee <- paste(ndformat(ffiitt$SE * 100, nd_num), "%", sep = "")
    
    # Update Z-statistic and p-value incorporating nh_normal
    z_val <- ffiitt$Z_Statistic
    tt <- round(z_val, 2)
    pv <- switch(alt_normal,
                 "greater" = pnorm(z_val, lower.tail = FALSE),
                 "less" = pnorm(z_val, lower.tail = TRUE),
                 "two.sided" = 2 * pnorm(-abs(z_val))
    )
    pval <- pvformat(pv)
    
  } else {
    ssee <- NA
    defr <- fit$parameter
    tt <- round(sqrt(fit$statistic), 2)
    pv <- fit$p.value
    pval <- pvformat(pv)
  }
  
  # Calculate DF and test statistic
  #defr <- fit$parameter
  #tt <- round(sqrt(fit$statistic), 2)
  
  # Format the p-value with boundary conditions.
  #pv <- fit$p.value
  #pval <- pvformat(pv)
  
  # --- 2. BUILD THE SUMMARY TABLE DATA AND HTML STRINGS ---
  
  dframe <- data.frame(
    Variable = refl,
    N1 = nn[1],
    Mis1 = mm[1],
    Mean1 = esti[1],
    N2 = nn[2],
    Mis2 = mm[2],
    Mean2 = esti[2]
  )
  row.names(dframe) <- NULL
  
  col_headers_html <- c(
    variable, "N", miss_text, "% (n)", "N", miss_text, "% (n)"
  )
  
  header_vector <- setNames(c(1, 3, 3), c(" ", group_levels[1], group_levels[2]))
  if (!miss_yn) {
    dframe <- dframe[, !grepl(miss_text, names(dframe))]
    col_headers_html <- col_headers_html[!col_headers_html %in% miss_text]
    header_vector <- setNames(c(1, 2, 2), c(" ", group_levels[1], group_levels[2]))
  }
  
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Descriptive Statistics</p>",
    sep = ""
  )
  
  # --- 3. BUILD THE KABLEEXTRA TABLE ---
  
  if (miss_yn) aa <- 1 else aa <- 0
  table_out_summ <- knitr::kable(
    dframe,
    format = "html",
    align = "c",
    col.names = col_headers_html,
    caption = caption_html,
    escape = FALSE
  ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    ) |>
    kableExtra::add_header_above(header_vector) |>
    kableExtra::column_spec(
      column = 1:dim(dframe)[2],
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
      border_left = "3px solid #666" 
    ) |>
    kableExtra::column_spec(
      column = 1,
      bold = TRUE
    ) |>
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd;; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::row_spec(
      row = 1,
      extra_css = "border-bottom: 2px solid #666; border-top: 1px solid #ddd;"
    ) 
  
  # --- 4. BUILD THE Inference DATA AND HTML STRINGS ---
  
  dframe <- data.frame(
    Variable = refl,
    diff = diff,
    Se = ssee,
    Ci = ci,
    Tt = tt,
    Pval = pval
  )
  row.names(dframe) <- NULL
  
  col_headers_html <- c(variable,"Diff", "SE Diff", 
                        paste(conf_normal * 100, "% CI for p<sub>1</sub> - p<sub>2</sub><sup>1</sup>", sep = ""),
                        "Z",
                        "P-value<sup>2</sup>"
  )
  
  if (!testyn_normal) {
    dframe$Pval <- NULL
    dframe$Tt <- NULL
    col_headers_html <- col_headers_html[!col_headers_html %in% c("Z", "P-value<sup>2</sup>")]
  }
  
  if (meth != "asymptotic") {
    dframe$Se <- NULL
    col_headers_html <- col_headers_html[!col_headers_html %in% "SE Diff"]
  }
  
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Test for Two Proportions</p>",
    sep = ""
  )
  
  ffnn1 <- if (meth == "asymptotic") "Interval method: Wald" else "Interval method: Score"
  
  if (testyn_normal) {
    fn1 <- ffnn1
    fn2 <- switch(alt_normal,
                  "greater" = paste0("Normal approximation; H<sub>1</sub>:p<sub>1</sub> - p<sub>2</sub>&gt; ", nh_normal),
                  "less" = paste0("Normal approximation; H<sub>1</sub>:p<sub>1</sub> - p<sub>2</sub>&lt; ", nh_normal),
                  "two.sided" = paste0("Normal approximation; H<sub>1</sub>:p<sub>1</sub> - p<sub>2</sub>&ne; ", nh_normal)   
                  )
    temp <- if (yatc) "with Yates correction" else ""
    if (temp != "") {
      fn2 <- paste(fn2, "; (", temp, ")", sep = "")
    }    
    fn1 <- paste("<i>", fn1, "<i>", sep = "")
    fn2 <- paste("<i>", fn2, "<i>", sep = "")
    footnotes_html <- c(fn1, fn2)
  } else {
    fn1 <- ffnn1
    fn1 <- paste("<i>", fn1, "<i>", sep = "")
    footnotes_html <- fn1
  }
  
  # --- 5. BUILD THE KABLEEXTRA TABLE ---
  
  table_out_infe <- knitr::kable(
    dframe,
    format = "html",
    align = "c",
    col.names = col_headers_html,
    caption = caption_html,
    escape = FALSE
  ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    ) |>
    kableExtra::column_spec(
      column = 1:dim(dframe)[2],
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    )|>
    kableExtra::column_spec(
      column = 1,
      bold = TRUE
    ) |>
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd;; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::row_spec(
      row = 1,
      extra_css = "border-bottom: 2px solid #666; border-top: 1px solid #ddd;"
    ) |>
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  # Return the table as a list.
  list(table_summ = table_out_summ, table_infe = table_out_infe)
}