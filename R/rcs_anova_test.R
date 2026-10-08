#' @title One-Way ANOVA Test and Summary Tables
#' @description This function performs a one-way ANOVA test, providing descriptive statistics,
#'   ANOVA results, and formatted HTML tables using base R and `data.table` (no `dplyr` or `broom`).
#'
#' @param data A `data.frame` or `tibble` containing the variables.
#' @param variable A character string specifying the name of the numeric variable
#'   (dependent variable) for the analysis.
#' @param by A character string specifying the name of the categorical variable
#'   (independent variable) used for grouping.
#' @param nd_num An integer specifying the number of decimal places for formatting numeric values.
#' @param ev A logical value (`TRUE` by default) to assume equal variances. If `FALSE`,
#'   the function will perform a Welch one-way ANOVA test.
#' @param font_size An integer specifying the font size for the output tables.
#' @param miss_yn A logical value (`FALSE` by default) to include a "missing" column
#'   in the descriptive statistics table.
#'
#' @return A `list` containing two elements:
#'   \itemize{
#'     \item \strong{table_summ}: A `kableExtra` HTML table with descriptive statistics.
#'     \item \strong{table_infe}: A `kableExtra` HTML table with the ANOVA results.
#'   }
#' @examples
#' # Example using the built-in `mtcars` dataset
#' # We will test if the number of cylinders affects miles per gallon
#' mtcars$cyl <- as.factor(mtcars$cyl)
#'
#' # Run the ANOVA test assuming equal variances
#' fit <- anova_results <- rcs_anova_test(
#'   data = mtcars,
#'   variable = "mpg",
#'   by = "cyl",
#'   nd_num = 2,
#'   ev = TRUE,
#'   font_size = 14
#' )
#'
#' # Access the tables and plot
#' fit$table_summ
#' fit$table_infe
#'
#' # Run the ANOVA test without assuming equal variances (Welch's test)
#' anova_welch_results <- rcs_anova_test(
#'   data = mtcars,
#'   variable = "mpg",
#'   by = "cyl",
#'   nd_num = 2,
#'   ev = FALSE,
#'   font_size = 14
#' )
#'
#' # Access the results from the Welch test
#' anova_welch_results$table_summ
#' anova_welch_results$table_infe
#'
#'
#'
#' @export
rcs_anova_test <- function(data, variable, by, nd_num = 1, ev = TRUE, font_size, miss_yn = FALSE,
                           miss_text = "Mis") {
  
  imis <- miss_yn
  nd <- nd_num
  
  # --- 1. PREPARE DATA AND CHECK SIZES ---
  dt <- data.table::as.data.table(data)
  
  var_data <- dt[[variable]]
  by_data <- factor(dt[[by]])
  
  temp <- table(by_data)
  if (any(temp < 2)) return(NULL)
  
  # Run the one-way ANOVA and extract summary table elements directly using base R
  fit <- stats::aov(var_data ~ by_data)
  aov_summary <- summary(fit)[[1]]
  
  fit1 <- data.table::data.table(
    term = trimws(rownames(aov_summary)),
    df = aov_summary[, "Df"],
    sumsq = aov_summary[, "Sum Sq"],
    meansq = aov_summary[, "Mean Sq"],
    statistic = aov_summary[, "F value"],
    p.value = aov_summary[, "Pr(>F)"]
  )
  
  # Calculate descriptive statistics using data.table
  fit2 <- dt[, .(
    nn = .N,
    mm = sum(is.na(get(variable))),
    me = mean(get(variable), na.rm = TRUE),
    sd = sd(get(variable), na.rm = TRUE)
  ), by = get(by)]
  data.table::setnames(fit2, 1, by)
  
  fit2[, `:=`(
    me = ndformat(me, nd),
    sd = ndformat(sd, nd)
  )]
  
  # Format the p-value with boundary conditions
  fit1[, p.value := pvformat(p.value)]
  
  # --- 2. BUILD THE SUMMARY TABLE DATA AND HTML STRINGS ---
  dframe_summ <- data.table::copy(fit2)
  
  col_headers_html <- c(by, "N", miss_text, "Mean", "SD")
  if (!imis) {
    dframe_summ[, nn := nn - mm]
    dframe_summ[, mm := NULL]
    col_headers_html <- c(by, "N", "Mean", "SD")
  }
  
  dframe_summ_df <- as.data.frame(dframe_summ)
  row.names(dframe_summ_df) <- NULL
  
  header_vector <- setNames(c(3, 2), c(" ", variable))
  if (!imis) {
    header_vector <- setNames(c(2, 2), c(" ", variable))
  }
  
  caption_html_summ <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Descriptive Statistics</p>",
    sep = ""
  )
  
  if (!imis) alit <- "lccc" else alit <- "lcccc"
  table_out_summ <- knitr::kable(
    dframe_summ_df,
    format = "html",
    align = alit,
    col.names = col_headers_html,
    caption = caption_html_summ,
    escape = FALSE
  ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    ) |>
    kableExtra::add_header_above(header_vector) |>
    kableExtra::column_spec(
      column = 1:dim(dframe_summ_df)[2],
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
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; padding-left: 10px; padding-right: 10px;"
    )
  
  # --- 3. BUILD THE INFERENCE DATA AND HTML STRINGS ---
  fit1[term == "by_data", term := "Between Groups"]
  fit1[term == "Residuals", term := "Within Groups"]
  
  dfrow <- data.table::data.table(
    term = "Total",
    df = sum(fit1$df, na.rm = TRUE),
    sumsq = sum(fit1$sumsq, na.rm = TRUE),
    meansq = NA_real_,
    statistic = NA_real_,
    p.value = NA_character_
  )
  fit1 <- rbind(fit1, dfrow, fill = TRUE)
  
  col_headers_infe <- c("Source", "df", "Sum of<br> Squares", "Mean Square", "F", "P-value")
  
  caption_html_infe <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>One-Way ANOVA test</p>",
    sep = ""
  )
  
  fit1[, `:=`(
    df = as.character(df),
    sumsq = ndformat(sumsq, 1),
    meansq = ndformat(meansq, 1),
    statistic = ndformat(statistic, 2)
  )]
  
  fit1[is.na(fit1)] <- ""
  
  dframe_infe_df <- as.data.frame(fit1)
  row.names(dframe_infe_df) <- NULL
  
  table_out_infe <- knitr::kable(
    dframe_infe_df,
    format = "html",
    align = "lccccc",
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
      column = 1:dim(dframe_infe_df)[2],
      border_left = "0.5px solid #ddd",
      border_right = "0.5px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::row_spec(
      row = c(2, 3),
      extra_css = "border-bottom: 1.5px solid #666; border-top: 1px solid #ddd;"
    )
  
  # --- 4. WELCH ANOVA HANDLING ---
  if (!ev) {
    fit_welch <- stats::oneway.test(var_data ~ by_data)
    
    dframe_welch <- data.frame(
      numdf = ndformat(fit_welch$parameter[1], 2),
      dendf = ndformat(fit_welch$parameter[2], 2),
      statistic = ndformat(fit_welch$statistic, 2),
      pvalue = pvformat(fit_welch$p.value)
    )
    
    col_headers_welch <- c("Numerator df", "Denominator df", "F", "P-value<sup>1</sup>")
    
    caption_html_welch <- paste(
      "<p style='text-align: left; margin-left: 0; font-size: ",
      font_size + 2,
      "px; color: maroon; font-weight: bold;'>Welch One-Way ANOVA Test</p>",
      sep = ""
    )
    
    table_out_infe <- knitr::kable(
      dframe_welch,
      format = "html",
      align = "lccccc",
      col.names = col_headers_welch,
      caption = caption_html_welch,
      escape = FALSE
    ) |>
      kableExtra::kable_styling(
        full_width = FALSE,
        position = "left",
        font_size = font_size
      ) |>
      kableExtra::column_spec(
        column = 1:dim(dframe_welch)[2],
        border_left = "1px solid #ddd",
        border_right = "1px solid #ddd",
        extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
      ) |>
      kableExtra::row_spec(
        row = 0,
        bold = TRUE,
        extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd; padding-left: 10px; padding-right: 10px;"
      ) |>
      kableExtra::row_spec(
        row = 1,
        extra_css = "border-bottom: 1.5px solid #666; border-top: 1px solid #ddd;"
      ) |>
      kableExtra::footnote(
        number = paste("<i>", fit_welch$method, "</i>"),
        escape = FALSE
      )
  }
  
  list(table_summ = table_out_summ, table_infe = table_out_infe)
}