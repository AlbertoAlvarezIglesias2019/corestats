#' @title Perform Two-Sample Proportions Intervals and Generate an HTML Table
#'
#' @description This function performs two-sample proportions comparisons for categorical variables
#' using base R and `data.table` (no `dplyr`). It builds a formatted HTML table and returns the underlying data.
#'
#' @param data A data frame containing the two categorical variables.
#' @param ccc A character string specifying the name of the first categorical variable (column variable).
#' @param rrr A character string specifying the name of the second categorical variable (row variable).
#' @param nd_num An integer specifying the number of decimal places to round numeric values. Defaults to 1.
#' @param font_size A numeric value for the font size of the table.
#' @param type A character string specifying the correction method (`"bonferroni"` or `"none"`).
#'
#' @return A `list` containing two elements: `table` (a `kableExtra` HTML table) and `data` (a `data.frame` of raw results).
#' @examples
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
#'   font_size = 14,
#'   type = "bonferroni"
#' )
#'
#'
#' data <- data.frame(level = factor(sample(c("A","B","C"),size=100,replace = TRUE)),treatment = factor(sample(c("Treat","Control","Other"),size=100,replace = TRUE)))
#'  
#' # Perform post-hoc tests with Holm correction
#' iii <- rcs_chisquare_intervals(data = data,ccc = "level",rrr = "treatment",font_size = 14,type = "bonferroni")
#' iii$data
#' iii$table
#' 
#' @export
rcs_chisquare_intervals <- function(data, ccc, rrr, nd_num = 1, font_size, type = "bonferroni") {
  
  nd <- nd_num
  
  # --- 1. PREPARE DATA ---
  df <- as.data.frame(data)
  df_clean <- df[!is.na(df[[rrr]]) & !is.na(df[[ccc]]), c(rrr, ccc), drop = FALSE]
  
  colu <- droplevels(factor(df_clean[[ccc]]))
  rows <- droplevels(factor(df_clean[[rrr]]))
  
  cl1 <- levels(colu)[1]
  if (length(levels(colu)) == 2) {
    cl2 <- levels(colu)[2]
  } else {
    cl2 <- paste(levels(colu)[-1], collapse = " or ")
  }
  
  colu1_vec <- ifelse(colu == levels(colu)[1], cl1, cl2)
  colu1 <- factor(colu1_vec, levels = c(cl1, cl2))
  
  xtab <- table(rows, colu1)
  
  # --- 2. SELECT CORRECTION AND ALPHA ---
  typelab <- data.table::fcase(
    type == "none", "Without correction",
    type == "bonferroni", "With Bonferroni correction",
    default = "Without correction"
  )
  
  alpha <- data.table::fcase(
    type == "none", 0.05,
    type == "bonferroni", 0.05 / nrow(xtab),
    default = 0.05
  )
  
  # --- 3. RUN PROPORTION TESTS ACROSS ROWS ---
  temp <- lapply(seq_len(nrow(xtab)), function(i) {
    fit <- prop.test(
      c(xtab[i, 1], xtab[i, 2]),
      c(sum(xtab[, 1]), sum(xtab[, 2])),
      conf.level = 1 - alpha
    )
    
    diff_val <- paste(ndformat((fit$estimate[1] - fit$estimate[2]) * 100, nd), "%", sep = "")
    out_str <- paste(ndformat(fit$conf.int * 100, nd), "%", sep = "")
    out_str <- paste(out_str, collapse = ", ")
    ci_val <- paste("(", out_str, ")", sep = "")
    pv <- fit$p.value
    pval <- pvformat(pv)
    
    data.frame(
      Lab = paste(row.names(xtab)[i], sep = ""),
      p1 = paste(ndformat(fit$estimate[1] * 100, nd), "%", sep = ""),
      p2 = paste(ndformat(fit$estimate[2] * 100, nd), "%", sep = ""),
      diff = diff_val,
      pe = fit$estimate[1] - fit$estimate[2],
      lb = fit$conf.int[1],
      ub = fit$conf.int[2],
      Ci = ci_val,
      Pval = pval,
      stringsAsFactors = FALSE
    )
  })
  
  DF <- do.call("rbind", temp)
  row.names(DF) <- NULL
  
  # --- 4. FORMAT TABLE ---
  dframe <- DF[, !(names(DF) %in% c("lb", "ub", "pe"))]
  col_headers_html <- c(rrr, levels(colu1)[1], levels(colu1)[2], "Diff in Prop", "95% CI<sup>1</sup>", "P-value<sup>1</sup>")
  
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Intervals Two-sample Proportions</p>",
    sep = ""
  )
  
  fn1 <- paste("<i>", typelab, "<i>", sep = "")
  footnotes_html <- fn1
  
  header_vector <- setNames(c(1, 2, 3), c(" ", ccc, " "))
  
  table_out_infe <- knitr::kable(
    dframe,
    format = "html",
    align = "lccccc",
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
      row = dim(dframe)[1],
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::row_spec(
      row = 1:dim(dframe)[1],
      extra_css = "border-top: 1px solid #ddd;"
    ) |>
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  list(table = table_out_infe, data = DF)
}
