#' Generate an HTML Table of Basic Descriptive Statistics (data.table Optimized)
#'
#' This function computes descriptive statistics for a specified variable,
#' optionally stratified by a grouping variable, and formats the results into
#' a highly-styled HTML table suitable for a "Table 1" in a research paper.
#' It relies on external, unprovided functions for core summary calculation
#' (\code{my_tbl_summary}) and p-value formatting (\code{pvformat}).
#'
#' @param data A data.frame or data.table containing the data.
#' @param outcome A character vector specifying the name(s) of the outcome(s) to summarize.
#' @param group_by A character string specifying the name of the grouping variable for stratification.
#'   Set to \code{NULL} (default) for no stratification.
#' @param miss_yn Logical. If \code{TRUE} (default), missing values are included in the table.
#' @param suca A string specifying the statistics to calculate for \strong{categorical} variables.
#'   Passed to the internal \code{my_tbl_summary} function. Defaults to \code{"nNp"}.
#' @param suco A string specifying the statistics to calculate for \strong{continuous} variables.
#'   Passed to the internal \code{my_tbl_summary} function. Defaults to \code{"meansd"}.
#' @param miss_text An optional character string to label the missing data rows.
#'   Defaults to \code{"(Unknown)"}.
#' @param addp Logical. If \code{TRUE} (default) and \code{group_by} is not \code{NULL}, a column for p-values is added.
#' @param addt Logical. If \code{TRUE} (default) and \code{group_by} is not \code{NULL}, an overall column (Total) is added.
#' @param addn Logical. If \code{TRUE} (default), the column for the total number of non-missing observations (\code{N}) is added.
#' @param outcome_text A string for the header of the first column, typically for variable names/labels (e.g., "Demographics"). Defaults to \code{"Demo"}.
#' @param groupby_text A string for the header spanning the columns of the \code{group_by} variable's levels (e.g., "Treatment"). Defaults to \code{"Treatment"}.
#' @param font_size An integer specifying the font size (in pixels) for the table body. Defaults to \code{16}.
#' @param main_title A string for the main title/caption of the table (e.g., "Table 1"). Defaults to \code{"Table 1"}.
#' @param nd_num An integer specifying the number of decimal places for numeric statistics. Defaults to \code{1}.
#' @param nd_cat An integer specifying the number of decimal places for categorical percentages. Defaults to \code{1}.
#'
#' @return A \code{list} containing the styled HTML table object (\code{kableExtra::kable_styling} output).
#'
#' @seealso \code{\link[knitr]{kable}}, \code{\link[kableExtra]{kable_styling}}
#'
#' @import data.table
#' @export
#'
#' @examples
#' # The example below is illustrative and assumes 'my_tbl_summary' and 'pvformat' exist.
#' \dontrun{
#' data(iris)
#' library(data.table)
#' setDT(iris)
#' iris[, trt := sample(rep(c("A","B"), each = .N/2))]
#' 
#' rcs_basicstatistics(
#'   data = iris,
#'   outcome = c("Sepal.Length", "Petal.Width", "Species"),
#'   group_by = "trt",
#'   main_title = "Iris Demographics by Treatment"
#' )
#' }
rcs_basicstatistics <- function(data, outcome, group_by = NULL,
                                miss_yn = TRUE,
                                miss_text = NULL,
                                suca = "nNp",
                                suco = "meansd",
                                addp = TRUE,
                                addt = TRUE,
                                addn = TRUE,
                                outcome_text = "Demo",
                                groupby_text = "Treatment",
                                font_size = 16,
                                main_title = "Table 1",
                                nd_num = 1,
                                nd_cat = 1) {
  
  miss_text <- if (!is.null(miss_text)) paste0("<i>", miss_text, "</i>")  else   paste0("<i>(Unknown)</i>") 
  

  # Prepare the table via the data.table-optimized summary function
  dframe <- data.table::as.data.table(
    my_tbl_summary(data, outcome, group_by, suca = suca, suco = suco, nd_num = nd_num, nd_cat = nd_cat)
  )
  
  # Format p-values securely
  dframe[, p.value := pvformat(p.value)]
  
  # Define the columns to map
  t1 <- c("row_type", "label", "test_name", "p.value", "variable", "var_type", "n", "stat_label")
  t2 <- grepl("Overall", names(dframe))
  statc <- names(dframe)[!(names(dframe) %in% t1) & !t2]
  overallc <- if (!is.null(group_by)) names(dframe)[t2] else statc
  
  # Change missing label directly in memory
  dframe[row_type == "missing", label := miss_text]
  
  # If no missing (represented by "0" in the primary count column), remove the row
  dframe <- dframe[!(row_type == "missing" & get(overallc[1]) == "0")]
  
  # Remove all missing rows if user opts out
  if (!miss_yn) dframe <- dframe[row_type != "missing"]
  
  # Construct final column output list
  collist <- "label"
  if (addn) collist <- c(collist, "n")
  if (addt && !is.null(group_by)) collist <- c(collist, overallc)
  
  collist <- unique(c(collist, statc))
  if (addp && !is.null(group_by)) collist <- c(collist, "p.value")
  
  # Fast in-memory conversion to character and NA replacement (Replaces slow dplyr::across)
  cols <- names(dframe)
  dframe[, (cols) := lapply(.SD, as.character)]
  dframe[, (cols) := lapply(.SD, function(x) data.table::fifelse(is.na(x), "", x))]
  
  # Tidy up the first column (labels/statistics) using vectorized assignments
  dframe[stat_label != "", stat_label := paste0("<span style='font-size: 0.8rem;'>", stat_label, "</span>")]
  dframe[row_type == "label", label := paste0("<b>", label, "</b>; ", stat_label)]
  dframe[row_type != "label", label := paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", label)]
  
  # Clean up dropped columns
  dframe[, c("row_type", "stat_label") := NULL]
  
  # Define the table column headers with HTML formatting
  col_headers_html <- collist
  col_headers_html[col_headers_html == "label"] <- outcome_text
  col_headers_html[col_headers_html == "n"] <- "N"
  col_headers_html[col_headers_html == "p.value"] <- "P-value<sup>1</sup>"
  
  # Create the HTML string for the table caption
  caption_html <- paste0(
    "<p style='text-align: center; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>", main_title, "</p>"
  )
  
  # Define the HTML strings for footnotes
  footnotes_html <- NULL
  if (addp && !is.null(group_by)) {
    # Extract unique tests used, ignoring empty strings generated from NA fill
    tests_used <- unique(dframe$test_name)
    tests_used <- tests_used[tests_used != ""]
    if (length(tests_used) > 0) {
      fn1 <- paste(tests_used, collapse = "; ")
      footnotes_html <- paste0("<i>", fn1, "</i>")
    }
  }
  
  # Define the HTML header vector for Groupings
  header_vector <- NULL
  if (!is.null(groupby_text) && !is.null(group_by) && groupby_text != "") {
    wher <- collist %in% statc
    tna <- data.table::fifelse(wher, groupby_text, " ")
    header_vector <- stats::setNames(rle(wher)$lengths, rle(tna)$values)
  }
  
  # Build text alignment flags
  alit <- paste0(c("l", rep("c", length(collist) - 1)), collapse = "")
  
  # --- BUILD THE KABLEEXTRA TABLE ---
  
  # 1. Base table (Pass as standard data.frame for maximal knitr compatibility)
  table_out <- knitr::kable(
    as.data.frame(dframe[, collist, with = FALSE]),
    format = "html",
    align = alit,
    col.names = col_headers_html,
    caption = caption_html,
    escape = FALSE
  )
  
  # 2. Add Styling
  table_out <- kableExtra::kable_styling(
    table_out,
    full_width = FALSE,
    position = "left",
    font_size = font_size
  )
  
  # 3. Apply Grouping Headers (if applicable)
  if (!is.null(header_vector)) {
    table_out <- kableExtra::add_header_above(table_out, header_vector)
  }
  
  # 4. Apply Borders / Rows
  row_count <- nrow(dframe)
  
  table_out <- kableExtra::row_spec(
    table_out,
    row = c(0, row_count),
    extra_css = "white-space: nowrap; border-bottom: 2px solid #666; padding-left: 10px; padding-right: 10px;"
  )
  
  table_out <- kableExtra::row_spec(
    table_out,
    row = row_count,
    extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd; padding-left: 10px; padding-right: 10px;"
  )
  
  table_out <- kableExtra::row_spec(
    table_out,
    row = 1:row_count,
    extra_css = "white-space: nowrap; border-top: 1px solid #ddd; padding-bottom: 5px; padding-top: 5px; padding-left: 10px; padding-right: 10px;"
  )
  
  # 5. Apply Footnotes
  if (!is.null(footnotes_html)) {
    table_out <- kableExtra::footnote(
      table_out,
      number = footnotes_html,
      escape = FALSE
    )
  }
  
  return(list(table = table_out))
}