#' @title Perform Post-Hoc Pairwise Proportions Tests and Generate an HTML Table
#'
#' @description This function performs pairwise comparisons of proportions for two
#' categorical variables using base R and `data.table` (no `dplyr` or `rstatix`).
#' It adjusts the p-values for multiple comparisons and presents the results in a styled HTML table.
#'
#' @param data A data frame containing the two categorical variables.
#' @param ccc A character string specifying the name of the first categorical variable (column variable).
#' @param rrr A character string specifying the name of the second categorical variable (row variable).
#' @param nd_num An integer specifying the number of decimal places to round numeric values. Defaults to 1.
#' @param font_size A numeric value for the font size of the table.
#' @param type A character string specifying the p-value adjustment method for multiple comparisons.
#'   Valid options are:
#'   \itemize{
#'     \item `"none"`: No p-value adjustment.
#'     \item `"holm"`: Holm (1979) method.
#'     \item `"hochberg"`: Hochberg (1988) method.
#'     \item `"hommel"`: Hommel (1988) method.
#'     \item `"bonferroni"`: Bonferroni correction.
#'   }
#'
#' @return A character string containing the HTML code for the formatted table.
#'
#' @examples
#'
#' # Create a sample data frame
#' data_df <- data.frame(
#'   gender = factor(c(rep("Male", 30), rep("Female", 20))),
#'   opinion = factor(c(rep("Agree", 15), rep("Disagree", 15), rep("Agree", 10), rep("Disagree", 10)))
#' )
#'
#' # Perform post-hoc tests with Bonferroni correction
#' rcs_chisquare_posthoc(
#'   data = data_df,
#'   ccc = "gender",
#'   rrr = "opinion",
#'   nd_num = 1,
#'   font_size = 14,
#'   type = "bonferroni"
#' )
#' @export
rcs_chisquare_posthoc <- function(data, ccc, rrr, nd_num = 1, font_size, type = "bonferroni") {
  
  if (is.function(data) || missing(data)) {
    stop("The 'data' argument must be a valid data frame or data.table.")
  }
  
  nd <- nd_num
  df <- as.data.frame(data)
  
  # Ensure the specified columns actually exist in the data frame
  if (!rrr %in% names(df)) {
    stop(paste("Row variable 'rrr' (", rrr, ") was not found in the data frame. Check your column names."))
  }
  if (!ccc %in% names(df)) {
    stop(paste("Column variable 'ccc' (", ccc, ") was not found in the data frame. Check your column names."))
  }
  
  # --- 1. PREPARE DATA ---
  df_clean <- df[!is.na(df[[rrr]]) & !is.na(df[[ccc]]), c(rrr, ccc), drop = FALSE]
  
  colu <- droplevels(factor(df_clean[[ccc]]))
  rows <- droplevels(factor(df_clean[[rrr]]))
  
  first_level <- levels(rows)[1]
  rows1 <- ifelse(rows == first_level, first_level, "OOtthheerr")
  rows1 <- factor(rows1, levels = c(first_level, "OOtthheerr"))
  
  xtab <- table(colu, rows1)
  
  # Choose the label for the type
  typelab <- data.table::fcase(
    type == "none", "None",
    type == "holm", "Holm",
    type == "hochberg", "Hochberg",
    type == "hommel", "Hommel",
    type == "bonferroni", "Bonferroni",
    default = "None"
  )
  
  # --- 2. PAIRWISE PROPORTION TESTS ---
  groups <- levels(colu)
  pairs <- utils::combn(groups, 2, simplify = FALSE)
  
  fit_list <- lapply(pairs, function(p) {
    g1 <- p[1]
    g2 <- p[2]
    x1 <- xtab[g1, 1]
    n1 <- sum(xtab[g1, ])
    x2 <- xtab[g2, 1]
    n2 <- sum(xtab[g2, ])
    res <- stats::prop.test(c(x1, x2), c(n1, n2), correct = FALSE)
    data.table::data.table(
      group1 = g1,
      group2 = g2,
      p = res$p.value
    )
  })
  fit <- data.table::rbindlist(fit_list)
  
  if (type != "none") {
    fit[, p.adj := stats::p.adjust(p, method = type)]
  } else {
    fit[, p.adj := p]
  }
  
  # --- 3. GET INTERVALS AND FORMAT TABLE ---
  fit1 <- getintervals_bc(data, ccc, rrr)
  fit1_dt <- data.table::as.data.table(fit1)
  
  fit_merged <- merge(fit1_dt, fit, by = c("group1", "group2"), all.x = TRUE)
  
  out <- data.table::copy(fit_merged)
  out[, contrast := paste(group1, group2, sep = " - ")]
  out[, dipro := paste(ndformat(estimate * 100, nd), "%", sep = "")]
  out[, ci := paste(paste(ndformat(conf.low * 100, nd), "%", sep = ""), 
                    paste(ndformat(conf.high * 100, nd), "%", sep = ""), 
                    sep = " to ")]
  out <- out[, .(contrast, dipro, ci, p.adj)]
  
  dframe <- data.table::copy(out)
  dframe[, p.adj := pvformat(p.adj)]
  dframe_df <- as.data.frame(dframe)
  row.names(dframe_df) <- NULL
  
  col_headers_html <- c("Contrast", "Diff in Prop<sup>1</sup>", "95% adjusted CI<sup>2</sup>", "Adjusted P-value<sup>3</sup>")
  
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Post-Hoc Pairwise comparisons</p>",
    sep = ""
  )
  
  fn1 <- paste("Proportion of ", rrr, " = ", first_level, sep = "")
  fn2 <- "Interval method: Asymptotic (with Bonferroni correction)"
  fn3 <- paste("Using a ", typelab, " correction", sep = "")
  fn1 <- paste("<i>", fn1, "<i>", sep = "")
  fn2 <- paste("<i>", fn2, "<i>", sep = "")
  fn3 <- paste("<i>", fn3, "<i>", sep = "")
  footnotes_html <- c(fn1, fn2, fn3)
  
  table_out_infe <- knitr::kable(
    dframe_df,
    format = "html",
    align = "lccc",
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
      column = 1:dim(dframe_df)[2],
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
      row = dim(dframe_df)[1],
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::row_spec(
      row = 1:dim(dframe_df)[1],
      extra_css = "border-top: 1px solid #ddd;"
    ) |>
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  return(table_out_infe)
}