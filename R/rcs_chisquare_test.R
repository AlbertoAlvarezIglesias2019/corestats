#' @title Perform Chi-Square and Fisher's Exact Tests and Generate HTML Tables
#'
#' @description This function performs a standard chi-squared test of independence
#' and Fisher's exact test for two categorical variables using base R and `data.table` (no `dplyr` or `tidyr`).
#' It returns a list of two separate, styled HTML tables: a detailed contingency table and a summary
#' of the test results.
#'
#' @param data A data frame containing the two categorical variables.
#' @param rrr A character string or unquoted name specifying the first categorical variable.
#' @param ccc A character string or unquoted name specifying the second categorical variable.
#' @param nd_num An integer specifying the number of decimal places for numeric values in the contingency table. Defaults to 1.
#' @param font_size A numeric value for the font size of the tables.
#' @param miss_yn A logical value indicating whether missing values should be included. Defaults to `FALSE`.
#' @param addfisher_yn A logical value indicating whether to include Fisher's exact test. Defaults to `FALSE`.
#'
#' @return A `list` containing two `knitr::kable` HTML tables:
#'   \itemize{
#'     \item `table_summ`: A contingency table showing observed counts, expected counts, and contributions to the chi-square statistic for each cell.
#'     \item `table_infe`: A summary table with the Chi-square statistic, degrees of freedom, and p-values for both Pearson's and Fisher's tests.
#'   }
#'
#' @examples
#'
#' # Create a sample data frame
#' data_df <- data.frame(
#'   rrr = factor(sample(c("A", "B", "C"), size = 100, replace = TRUE)),
#'   ccc = factor(sample(c("Treat", "Control"), size = 100, replace = TRUE))
#' )
#'
#' # Generate the chi-square and Fisher's tables
#' chi_tables <- rcs_chisquare_test(
#'   data = data_df,
#'   rrr = "rrr",
#'   ccc = "ccc",
#'   nd_num = 2,
#'   font_size = 14,
#'   addfisher_yn = TRUE,
#'   rows_text="RR",
#'   cols_text="CC"
#' )
#'
#' # Display the tables
#' print(chi_tables$table_summ)
#' print(chi_tables$table_infe)
#' @export
rcs_chisquare_test <- function(data, rrr, ccc, nd_num = 1, font_size, miss_yn = FALSE, addfisher_yn = FALSE,
                               miss_text=NULL,rows_text=NULL,cols_text=NULL) {
  
  # Ensure data is valid and not a function/closure
  if (is.function(data) || missing(data)) {
    stop("The 'data' argument must be a valid data frame or data.table.")
  }
  
  # Handle both quoted strings and unquoted column names for rrr and ccc
  rrr_expr <- substitute(rrr)
  if (is.symbol(rrr_expr) || is.call(rrr_expr)) rrr <- deparse(rrr_expr)
  
  ccc_expr <- substitute(ccc)
  if (is.symbol(ccc_expr) || is.call(ccc_expr)) ccc <- deparse(ccc_expr)
  
  imis <- miss_yn
  nd <- nd_num
  
  # --- 1. PREPARE DATA ---
  df <- as.data.frame(data)
  
  # Filter out NAs safely using base R
  df_clean <- df[!is.na(df[[rrr]]) & !is.na(df[[ccc]]), c(rrr, ccc), drop = FALSE]
  
  var_data <- droplevels(factor(df_clean[[rrr]]))
  by_data <- droplevels(factor(df_clean[[ccc]]))
  
  ttt1 <- table(var_data, by_data)
  ttt2 <- addmargins(ttt1)
  
  fit <- stats::chisq.test(ttt1)
  
  # Robust matrix-to-data.table conversion for expected counts
  expe_mat <- fit$expected
  expe_dt <- data.table::data.table(
    var_data = rep(rownames(expe_mat), times = ncol(expe_mat)),
    by_data = rep(colnames(expe_mat), each = nrow(expe_mat)),
    Expe = as.vector(expe_mat)
  )
  
  # Robust matrix-to-data.table conversion for chi-square contributions
  cont_mat <- (fit$expected - fit$observed)^2 / fit$expected
  cont_dt <- data.table::data.table(
    var_data = rep(rownames(cont_mat), times = ncol(cont_mat)),
    by_data = rep(colnames(cont_mat), each = nrow(cont_mat)),
    Cont = as.vector(cont_mat)
  )
  
  MIS <- paste0("<i>",miss_text,"</i>")
  
  if (imis) {
    cross_res <- my_tbl_cross(data, rrr, ccc)
    cross_dt <- data.table::as.data.table(cross_res)[-1]
    cross_dt[, c("test_name", "p.value") := NULL]
    
    out_dt <- data.table::melt(cross_dt, id.vars = "RRR", variable.name = "by_data", value.name = "Freq")
    data.table::setnames(out_dt, "RRR", "var_data")
    
    
    out_dt[var_data == "MMM", var_data := MIS]
    out_dt[by_data == "MMM", by_data := MIS]
  } else {
    ttt2_mat <- as.matrix(ttt2)
    out_dt <- data.table::data.table(
      var_data = rep(rownames(ttt2_mat), times = ncol(ttt2_mat)),
      by_data = rep(colnames(ttt2_mat), each = nrow(ttt2_mat)),
      Freq = as.vector(ttt2_mat)
    )
  }
  
  out_dt <- merge(out_dt, expe_dt, by = c("var_data", "by_data"), all.x = TRUE,sort=FALSE)
  out_dt <- merge(out_dt, cont_dt, by = c("var_data", "by_data"), all.x = TRUE,sort=FALSE)
  
  
  
  out_dt[, `:=`(
    Expe = ndformat(Expe, nd),
    Cont = ndformat(Cont, nd+1)
  )]
  

  out_dt[, cellval := paste0(Freq, "<br>&nbsp;<br>&nbsp;")]
  out_dt[!is.na(Expe) & Expe != " " & Expe != "", cellval := paste(Freq, Expe, Cont, sep = "<br>")]
  out_dt[var_data %in% c(MIS, "Sum"), cellval := Freq]
  out_dt[!var_data %in% c(MIS, "Sum"), var_data := paste0(var_data, "<br>&nbsp;<br>&nbsp;")]
  

  out_dt <- out_dt[, .(var_data, by_data, cellval)]
  
  out_dt[var_data != "Sum", var_data := paste("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", var_data, sep = "")]
  out_dt[var_data == "Sum", var_data := "<b>All</b>"]
  out_dt[by_data == "Sum", by_data := "<b>All</b>"]
  
  # Preserve exact appearance order for dcast
  out_dt[, var_data := factor(var_data, levels = unique(var_data))]
  out_dt[, by_data  := factor(by_data,  levels = unique(by_data))]
  
  dframe <- data.table::dcast(out_dt, var_data ~ by_data, value.var = "cellval")
  dframe <- as.data.frame(dframe)
  

  dframe[] <- lapply(dframe, as.character)

  # Add a first blank row
  temp <- dframe[1, , drop = FALSE]
  temp[1, ] <- ""
  temp[1, 1] <- paste("<b>", rows_text, "</b>", sep = "")
  dframe <- rbind(temp, dframe)
  
  col_headers_html <- names(dframe)
  col_headers_html[col_headers_html == "var_data"] <- " "
  
  caption_html <- paste(
    "<p style='text-align: center; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Contingency Table</p>",
    sep = ""
  )
  
  footnotes_html <- "&nbsp;&nbsp;&nbsp;<i>Count</i><br>&nbsp;&nbsp;&nbsp;<i>Expected count</i><br>&nbsp;&nbsp;&nbsp;<i>Contribution to Chi-square</i>"
  
  wher <- !names(dframe) %in% c("var_data", "<b>All</b>")
  tna <- names(dframe)
  tna <- ifelse(wher, cols_text, " ")
  header_vector <- setNames(rle(wher)$lengths, rle(tna)$values)
  
  alit <- c("l", rep("l", dim(dframe)[2] - 1))
  alit <- paste(alit, collapse = "")
  
  table_out_summ <- knitr::kable(
    as.data.frame(dframe),
    format = "html",
    align = alit,
    col.names = col_headers_html,
    caption = caption_html,
    bold = FALSE,
    escape = FALSE
  ) |>
    kableExtra::row_spec(
      row = 0,
      extra_css = "font-weight: normal;"
    ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    ) |>
    kableExtra::add_header_above(header_vector) |>
    kableExtra::row_spec(
      row = c(0, dim(dframe)[1]),
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::row_spec(
      row = dim(dframe)[1],
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::row_spec(
      row = 1:dim(dframe)[1],
      extra_css = "white-space: nowrap; border-top: 1px solid #ddd; padding-bottom: 5px; padding-top: 5px; padding-left: 10px; padding-right: 10px;"
    ) |>
    kableExtra::footnote(
      general = footnotes_html,
      general_title = "Cell Contents",
      escape = FALSE
    )
  
  # --- 2. CHI-SQUARE AND FISHER TESTS ---
  expected_counts <- as.vector(fit$expected)
  messa <- NULL
  if (any(expected_counts < 5)) {
    messa <- "<i>Warning: Expected counts are less than 5.<br> Chi-Square test may be unreliable</i>"
  }
  
  dframe_infe <- data.frame(
    chisq = ndformat(fit$statistic, nd+1),
    df = fit$parameter,
    pv1 = pvformat(fit$p.value)
  )
  row.names(dframe_infe) <- NULL
  
  col_headers_infe <- c("Chi-Square", "DF", "P-value<sup>1</sup>")
  footnotes_infe <- "<i>Pearson's Chi-squared test</i>"
  
  if (addfisher_yn) {
    fit2 <- tryCatch(
      {
        stats::fisher.test(ttt1)
      },
      error = function(e) {
        if (grepl("FEXACT error", conditionMessage(e))) {
          stats::fisher.test(ttt1, simulate.p.value = TRUE)
        } else {
          stop(e)
        }
      }
    )
    
    dframe_infe$pv2 <- pvformat(fit2$p.value)
    col_headers_infe <- c(col_headers_infe, "P-value<sup>2</sup>")
    fn2 <- ifelse(
      fit2$method == "Fisher's Exact Test for Count Data",
      "<i>Fisher's Exact Test for Count Data</i>",
      "<i>Fisher's Exact Test for Count Data (Monte Carlo simulation)</i>"
    )
    footnotes_infe <- c(footnotes_infe, fn2)
  }
  
  caption_html_infe <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Chi-Square Test</p>",
    sep = ""
  )
  
  table_out_infe <- knitr::kable(
    dframe_infe,
    format = "html",
    align = "c",
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
      column = 1:dim(dframe_infe)[2],
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
      extra_css = "border-bottom: 1px solid #666; border-top: 1px solid #ddd;"
    ) |>
    kableExtra::footnote(
      number = footnotes_infe,
      escape = FALSE
    )
  
  if (!is.null(messa)) {
    table_out_infe <- table_out_infe |> kableExtra::footnote(
      general = messa,
      general_title = "",
      escape = FALSE
    )
  }
  
  list(table_summ = table_out_summ, table_infe = table_out_infe,nc=ncol(dframe)-1)
}
