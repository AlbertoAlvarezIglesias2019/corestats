#' @title Generate Formatted HTML Cross-Tabulation Table
#'
#' @description
#' Creates a highly styled HTML cross-tabulation table using \code{knitr::kable} and \code{kableExtra}.
#' It internally relies on a custom function \code{my_tbl_cross} for the initial tabulation
#' and handles options for percentages, marginal totals, and missing values.
#' This function has been optimized to use \code{data.table} for fast, dependency-minimal data manipulation.
#'
#' @param data A data frame or data.table containing the data.
#' @param row A character string specifying the name of the column to use for the table rows.
#' @param col A character string specifying the name of the column to use for the table columns.
#' @param percent A character string specifying the type of percentage to display.
#'   Possible values include:
#'   \itemize{
#'     \item \code{"none"} (default)
#'     \item \code{"row"} (row percentages)
#'     \item \code{"colu"} (column percentages)
#'     \item \code{"cell"} (cell percentages)
#'   }
#' @param marg A character string specifying which marginal totals to include.
#'   Possible values are:
#'   \itemize{
#'     \item \code{"mnone"} (default, no marginals)
#'     \item \code{"mcolu"} (column marginals only)
#'     \item \code{"mrow"} (row marginals only)
#'     \item \code{"mcolrow"} (both column and row marginals)
#'   }
#' @param miss_yn Logical. If \code{TRUE} (default), include missing values in the table.
#' @param miss_text An optional character string to replace the default missing value label ("Unknown").
#' @param margin_text An optional character string to replace the default margin total label ("Total").
#' @param addp Logical. If \code{TRUE} (default), include the p-value from the underlying
#'   \code{my_tbl_cross} test, formatted by \code{pvformat}.
#' @param font_size Numeric. The base font size for the table, in pixels (default is 16).
#' @param main_title Character string. The main title or caption for the table, displayed with custom HTML styling.
#' @param nd_cat Integer. Number of decimals for percentages Default is \code{1}.
#'
#' @return A list containing the generated HTML table object, suitable for display in R Markdown
#'   or Shiny applications.
#'   \item{table}{An object of class \code{knitr_kable} and \code{kableExtra} that can be printed
#'     or further manipulated.}
#' @export
#'
#' @importFrom data.table as.data.table setnames set copy := fifelse
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling row_spec add_header_above footnote
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_tbl_cross' and 'pvformat' functions are defined and available,
#' # and a data frame 'iris' is used.
#'
#' # Sample data (using a built-in R dataset for demonstration)
#' data(iris)
#' iris$Petal.Length_Group <- cut(iris$Petal.Length, breaks = c(0, 3, 5, 7),
#'                                labels = c("Short", "Medium", "Long"), include.lowest = TRUE)
#'
#' # Example 1: Basic table with column percentages and column marginals
#' table_html_1 <- rcs_tables(
#'   data = iris,
#'   row = "Species",
#'   col = "Petal.Length_Group",
#'   percent = "colu",
#'   marg = "mcolu",
#'   main_title = "Species by Petal Length Group (Col Percent)",
#'   font_size = 14
#' )
#' print(table_html_1$table) # To display in R console/viewer
#'
#' # Example 2: Table without p-value, with row percentages and both marginals
#' table_html_2 <- rcs_tables(
#'   data = iris,
#'   row = "Species",
#'   col = "Petal.Length_Group",
#'   percent = "row",
#'   marg = "mcolrow",
#'   addp = FALSE,
#'   margin_text = "TOTAL",
#'   main_title = "Species by Petal Length Group (Row Percent)"
#' )
#' print(table_html_2$table)
#'
#' # Example 3: Basic table without marginals and including missing text
#' # (Must simulate missing data for this example to have effect)
#' iris_miss <- iris
#' iris_miss$Petal.Length_Group[10:15] <- NA
#'
#' table_html_3 <- rcs_tables(
#'   data = iris_miss,
#'   row = "Species",
#'   col = "Petal.Length_Group",
#'   percent = "none",
#'   marg = "mnone",
#'   miss_yn = TRUE,
#'   miss_text = "NA Value",
#'   addp = TRUE,
#'   main_title = "Species by Petal Length Group (Including Missing)"
#' )
#' print(table_html_3$table)
#' }
rcs_tables <- function(data,
                       row,
                       col,
                       percent = "none",
                       marg = "mcolrow",
                       miss_yn = TRUE,
                       miss_text = NULL,
                       margin_text = NULL,
                       cols_text = NULL,
                       rows_text = NULL,
                       addp = TRUE,
                       font_size = 16,
                       main_title = NULL,
                       nd_cat = 1) {
  
  # Define labels for missing and margin totals
  #miss_text <- if (!is.null(miss_text) & !miss_text=="") paste0("<i>", miss_text, "</i>")  else   paste0("<i>(Unknown)</i>")  
  
  #margin_text <- if (!is.null(margin_text) & !margin_text=="") margin_text  else "Overall"
  #cols_text <- if (!is.null(cols_text)& !cols_text=="") cols_text  else col
  #rows_text <- if (!is.null(rows_text)& !rows_text=="") rows_text  else row
  
  
  # Define labels for missing and margin totals
  miss_text <- if (!is.null(miss_text) && length(miss_text) > 0 && miss_text != "") paste0("<i>", miss_text, "</i>") else paste0("<i>(Unknown)</i>")  
  
  margin_text <- if (!is.null(margin_text) && length(margin_text) > 0 && margin_text != "") margin_text else "Overall"
  cols_text <- if (!is.null(cols_text) && length(cols_text) > 0 && cols_text != "") cols_text else col
  rows_text <- if (!is.null(rows_text) && length(rows_text) > 0 && rows_text != "") rows_text else row
  
  # +++++++++++++++++++++++
  # +++ Prepare the table
  # +++++++++++++++++++++++
  TTT_raw <- my_tbl_cross(data, row = row, col = col, perc = percent, nd = nd_cat)
  tbl <- data.table::as.data.table(TTT_raw)
  tbl[,"RRR"][1] <- rows_text
  
  # If percent is "none", counts are stored as characters. Convert them to numeric for arithmetic operations.
  if (percent == "none") {
    num_cols <- setdiff(names(tbl), c("RRR", "test_name", "p.value"))
    for (j in num_cols) {
      if (j %in% names(tbl)) {
        tbl[, (j) := as.numeric(get(j))]
      }
    }
  }
  
  # Store test_name for footnote before safely dropping it
  fn_test_name <- NULL
  if ("test_name" %in% names(tbl)) {
    fn_test_name <- tbl[["test_name"]][1]
    tbl[, test_name := NULL]
  }
  
  # RRR seems to be the row label column in the output of my_tbl_cross
  if ("RRR" %in% names(tbl)) {
    first_rrr <- tbl[["RRR"]][1]
    tbl[, row_type := data.table::fifelse(RRR == "Sum" | RRR == first_rrr, "label", "nolabel")]
  }
  
  # Include missing
  if (miss_yn) {
    if ("RRR" %in% names(tbl)) {
      tbl[RRR == "MMM", RRR := miss_text]
    }
    if ("MMM" %in% names(tbl)) {
      data.table::setnames(tbl, "MMM", miss_text)
    }
  } else {
    if (percent == "none" && "MMM" %in% names(tbl) && "Sum" %in% names(tbl)) {
      # Adjust total row/column if 'MMM' is being removed without percentages
      tbl[, Sum := Sum - MMM]
      
      sum_idx <- which(tbl[["RRR"]] == "Sum")
      mmm_idx <- which(tbl[["RRR"]] == "MMM")
      
      # Subtract MMM row values from Sum row values for all data columns safely
      if (length(sum_idx) == 1 && length(mmm_idx) == 1) {
        cols_to_update <- setdiff(names(tbl), c("RRR", "p.value", "row_type"))
        for (j in cols_to_update) {
          val_sum <- as.numeric(tbl[[j]][sum_idx])
          val_mmm <- as.numeric(tbl[[j]][mmm_idx])
          if (!is.na(val_sum) && !is.na(val_mmm)) {
            data.table::set(tbl, i = sum_idx, j = j, value = val_sum - val_mmm)
          }
        }
      }
    }
    # Drop MMM rows and columns
    if ("RRR" %in% names(tbl)) tbl <- tbl[RRR != "MMM"]
    if ("MMM" %in% names(tbl)) tbl[, MMM := NULL]
  }
  
  # Deal with the margins
  if (marg == "mnone") {
    if ("RRR" %in% names(tbl)) tbl <- tbl[RRR != "Sum"]
    if ("Sum" %in% names(tbl)) tbl[, Sum := NULL]
  } else if (marg == "mcolu") {
    if ("Sum" %in% names(tbl)) tbl[, Sum := NULL]
    if ("RRR" %in% names(tbl)) tbl[RRR == "Sum", RRR := margin_text]
  } else if (marg == "mrow") {
    if ("RRR" %in% names(tbl)) tbl <- tbl[RRR != "Sum"]
    if ("Sum" %in% names(tbl)) data.table::setnames(tbl, "Sum", margin_text)
  } else if (marg == "mcolrow") {
    if ("RRR" %in% names(tbl)) tbl[RRR == "Sum", RRR := margin_text]
    if ("Sum" %in% names(tbl)) data.table::setnames(tbl, "Sum", margin_text)
  }
  
  # p.value
  if (addp && "p.value" %in% names(tbl)) {
    first_pval <- pvformat(tbl[["p.value"]][1])
    tbl[, p.value := as.character(p.value)] 
    tbl[1, p.value := first_pval]
    if (nrow(tbl) > 1) {
      tbl[2:nrow(tbl), p.value := NA_character_]
    }
  } else {
    if ("p.value" %in% names(tbl)) tbl[, p.value := NULL]
  }
  
  # Make a strict copy for styling
  dframe <- data.table::copy(tbl)
  
  # Convert everything to character and replace NA with empty string safely
  col_names <- names(dframe)
  for (j in col_names) {
    data.table::set(dframe, j = j, value = as.character(dframe[[j]]))
    
    na_idx <- which(is.na(dframe[[j]]))
    if (length(na_idx) > 0) {
      data.table::set(dframe, i = na_idx, j = j, value = "")
    }
  }
  
  # *************************
  # *** tidy up first column
  # *************************
  if ("RRR" %in% names(dframe) && "row_type" %in% names(dframe)) {
    dframe[row_type == "label", RRR := paste0("<b>", RRR, "</b>")]
    dframe[row_type != "label", RRR := paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", RRR)]
    dframe[, row_type := NULL]
  }
  
  # *************************
  # Define the table column headers with HTML formatting.
  # *************************
  col_headers_html <- names(dframe)
  col_headers_html[col_headers_html == "RRR"] <- " "
  col_headers_html[col_headers_html == "p.value"] <- "<b>P-value<sup>1</sup></b>"
  col_headers_html[col_headers_html == margin_text] <- paste0("<b>", margin_text, "</b>")
  
  # *************************
  # Create the HTML string for the table caption.
  # *************************
  caption_html <- paste0(
    "<p style='text-align: center; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>", main_title, "</p>"
  )
  
  # *************************
  # Define the HTML strings for footnotes
  # *************************
  footnotes_html <- NULL
  if (addp && !is.null(fn_test_name)) {
    footnotes_html <- paste0("<i>", fn_test_name, "</i>")
  }
  
  # *************************
  # Define the HTML header vector for spanning columns
  # *************************
  ttmm <- names(TTT_raw)[!names(TTT_raw) %in% c("RRR", "MMM", "Sum", "test_name", "p.value")]
  wher <- names(dframe) %in% ttmm
  tna <- data.table::fifelse(wher, cols_text, " ")
  header_vector <- setNames(rle(tna)$lengths, rle(tna)$values)
  
  # Alignment string for kable
  alit <- paste0("l", paste(rep("c", ncol(dframe) - 1), collapse = ""))
  
  # ---  BUILD THE KABLEEXTRA TABLE ---
  table_out <- knitr::kable(
    as.data.frame(dframe),
    format = "html",
    align = alit,
    col.names = col_headers_html,
    caption = caption_html,
    bold = FALSE,
    escape = FALSE
  )
  
  # Apply headers
  table_out <- kableExtra::add_header_above(table_out, header_vector)
  
  # Base styling
  table_out <- kableExtra::kable_styling(
    table_out,
    full_width = FALSE,
    position = "left",
    font_size = font_size
  ) 
  
  # Header Row Specs (with font-weight: normal for column levels)
  table_out <- kableExtra::row_spec(
    table_out,
    row = 0,
    extra_css = "white-space: nowrap; border-bottom: 2px solid #666; padding-bottom: 5px; padding-top: 5px; padding-left: 10px; padding-right: 10px; font-weight: normal;"
  )
  
  if (nrow(dframe) > 0) {
    # Data Row Specs
    table_out <- kableExtra::row_spec(
      table_out,
      row = 1:nrow(dframe),
      extra_css = "white-space: nowrap; border-top: 1px solid #ddd; padding-bottom: 5px; padding-top: 5px; padding-left: 10px; padding-right: 10px;"
    )
    
    # Final Row Specs (thicker bottom border)
    table_out <- kableExtra::row_spec(
      table_out,
      row = nrow(dframe),
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; padding-left: 10px; padding-right: 10px;"
    )
  }
  
  if (!is.null(footnotes_html)) {
    table_out <- kableExtra::footnote(
      table_out,
      number = footnotes_html,
      escape = FALSE
    )
  }
  
  return(list(table = table_out))
}