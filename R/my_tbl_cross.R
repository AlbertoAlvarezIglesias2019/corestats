#' @title Internal Cross-Tabulation with Percentages and Statistical Test
#'
#' @description
#' An internal utility function to create a cross-tabulation table from two
#' variables, handle missing values, calculate cell contents based on a percentage
#' type, and perform a Chi-squared or Fisher's exact test. It prepares the data
#' in a wide format for subsequent formatting functions like \code{tables____html}.
#' This function has been optimized to use \code{data.table} for zero-dependency speed.
#'
#' @param data A data frame, tibble, or data.table.
#' @param row A character string specifying the name of the column for the rows.
#' @param col A character string specifying the name of the column for the columns.
#' @param perc A character string specifying the type of percentage to include
#'   in the cell content. Default is \code{"none"}.
#'   \itemize{
#'     \item \code{"none"}: Counts only.
#'     \item \code{"colu"}: Column percentages.
#'     \item \code{"row"}: Row percentages.
#'     \item \code{"cell"}: Cell percentages.
#'   }
#' @param nd Integer. Number of decimals for percentages. Default is \code{1}.
#'
#' @details
#' The function converts missing values (\code{NA}) to a special factor level "MMM"
#' to include them in the initial tabulation. It then calculates the requested
#' percentages based on the non-missing total for the relevant margin (or overall
#' total for cell percentages).
#'
#' It performs a statistical test (Chi-squared or Fisher's Exact) on the table
#' *excluding* missing values, based on the expected cell count rule (5 is the threshold).
#'
#' The output structure includes special rows/columns for Sums and MMM (Missing),
#' as well as the test name and p-value.
#'
#' @return A data.table in wide format where:
#' \itemize{
#'   \item The first column is \code{RRR} (Row variable levels).
#'   \item Subsequent columns are the levels of the \code{CCC} (Column variable levels),
#'         containing the count and percentage string (e.g., "10 (20%)").
#'   \item Includes special rows/columns for \code{Sum} and \code{MMM}.
#'   \item Two final columns: \code{test_name} (character string) and \code{p.value} (numeric).
#' }
#' @export
#'
#' @importFrom base table addmargins data.frame names all suppressWarnings as.character
#' @importFrom stats chisq.test fisher.test
#' @importFrom data.table as.data.table data.table set dcast merge.data.table fifelse :=
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_tbl_cross' is part of an R package.
#' data(iris)
#'
#' # Convert Species to character and add some NA for demonstration
#' iris$Species_char <- as.character(iris$Species)
#' iris$Species_char[1:5] <- NA
#' 
#' # Convert Petal.Length to a factor with NAs
#' iris$PL_grp <- cut(iris$Petal.Length, breaks = c(0, 3, 5, 7),
#'                    labels = c("Short", "Medium", "Long"), include.lowest = TRUE)
#' iris$PL_grp[c(3,51:55)] <- NA
#'
#' # Example 1: Column percentages
#' tab_col <- my_tbl_cross(iris, row = "PL_grp", col = "Species_char", perc = "colu")
#' # print(tab_col)
#'
#' # Example 2: No percentages (counts only)
#' tab_none <- my_tbl_cross(iris, row = "PL_grp", col = "Species_char", perc = "none")
#' # print(tab_none)
#'
#' # Example 3: Cell percentages
#' tab_cell <- my_tbl_cross(iris, row = "PL_grp", col = "Species_char", perc = "cell")
#' # print(tab_cell)
#' }
my_tbl_cross <- function(data, row, col, perc = "none", nd = 1) {
  
  # Ensure we have a data.table and extract target columns
  DT <- data.table::as.data.table(data)
  tmpDat <- data.table::data.table(
    CCC = DT[[col]], 
    RRR = DT[[row]]
  )
  
  ndp <- nd
  
  # 1. Prepare data and handle missing values safely
  tmpDat[, CCC := base::addNA(as.factor(CCC))]
  tmpDat[, RRR := base::addNA(as.factor(RRR))]
  
  # Rename NA level to "MMM" (Missing)
  base::levels(tmpDat$CCC)[base::is.na(base::levels(tmpDat$CCC))] <- "MMM"
  base::levels(tmpDat$RRR)[base::is.na(base::levels(tmpDat$RRR))] <- "MMM"
  
  # 2. Initial Cross-tabulation with Margins
  ddd_table <- base::table(tmpDat$CCC, tmpDat$RRR, deparse.level = 0)
  ddd_table <- stats::addmargins(ddd_table)
  
  # Convert to data.table
  ddd1 <- base::as.data.frame(ddd_table, responseName = "Freq")
  base::names(ddd1)[1:2] <- c("CCC", "RRR")
  data.table::setDT(ddd1)
  
  # Keep track of original column and row order to maintain layout in dcast
  lvls_ccc <- unique(ddd1$CCC)
  lvls_rrr <- unique(ddd1$RRR)
  
  # 3. Calculate Cell Content (Counts or Counts + Percentage)
  if (perc == "none") {
    ddd1[, value := base::as.character(Freq)]
  }
  
  if (perc == "colu") {
    tt1 <- ddd1[RRR == "Sum", .(CCC, N = Freq)]
    tt2 <- ddd1[RRR == "MMM", .(CCC, MIS = Freq)]
    
    ddd1 <- data.table::merge.data.table(ddd1, tt1, by = "CCC", all.x = TRUE)
    ddd1 <- data.table::merge.data.table(ddd1, tt2, by = "CCC", all.x = TRUE)
    ddd1[is.na(MIS), MIS := 0] # Replace NA if MMM doesn't exist
    
    # Recalculate 'Sum' row to be non-missing total
    ddd1[RRR == "Sum" & CCC != "MMM", Freq := N - MIS]
    
    # Create the display value: Freq (Percentage)
    ddd1[, value := data.table::fifelse(
      CCC == "MMM" | RRR == "MMM",
      base::as.character(Freq),
      base::paste0(Freq, " (", ndformat(Freq / (N - MIS) * 100, ndp), "%)")
    )]
  }
  
  if (perc == "row") {
    tt1 <- ddd1[CCC == "Sum", .(RRR, N = Freq)]
    tt2 <- ddd1[CCC == "MMM", .(RRR, MIS = Freq)]
    
    ddd1 <- data.table::merge.data.table(ddd1, tt1, by = "RRR", all.x = TRUE)
    ddd1 <- data.table::merge.data.table(ddd1, tt2, by = "RRR", all.x = TRUE)
    ddd1[is.na(MIS), MIS := 0]
    
    # Recalculate 'Sum' column to be non-missing total
    ddd1[CCC == "Sum" & RRR != "MMM", Freq := N - MIS]
    
    # Create the display value: Freq (Percentage)
    ddd1[, value := data.table::fifelse(
      CCC == "MMM" | RRR == "MMM",
      base::as.character(Freq),
      base::paste0(Freq, " (", ndformat(Freq / (N - MIS) * 100, ndp), "%)")
    )]
  }
  
  if (perc == "cell") {
    # Get column-wise totals
    tt1_ccc <- ddd1[RRR == "Sum", .(CCC, Nccc = Freq)]
    tt2_ccc <- ddd1[RRR == "MMM", .(CCC, MISccc = Freq)]
    
    # Get row-wise totals
    tt1_rrr <- ddd1[CCC == "Sum", .(RRR, Nrrr = Freq)]
    tt2_rrr <- ddd1[CCC == "MMM", .(RRR, MISrrr = Freq)]
    
    ddd1 <- data.table::merge.data.table(ddd1, tt1_ccc, by = "CCC", all.x = TRUE)
    ddd1 <- data.table::merge.data.table(ddd1, tt2_ccc, by = "CCC", all.x = TRUE)
    ddd1 <- data.table::merge.data.table(ddd1, tt1_rrr, by = "RRR", all.x = TRUE)
    ddd1 <- data.table::merge.data.table(ddd1, tt2_rrr, by = "RRR", all.x = TRUE)
    
    ddd1[is.na(MISccc), MISccc := 0]
    ddd1[is.na(MISrrr), MISrrr := 0]
    
    mis_overlap <- ddd1[CCC == "MMM" & RRR == "MMM", Freq]
    misinboth <- if (length(mis_overlap) > 0) mis_overlap else 0
    
    ddd1[, newfreq := Freq]
    ddd1[CCC == "Sum" & RRR != "MMM" & RRR != "Sum", newfreq := Nrrr - MISrrr]
    ddd1[RRR == "Sum" & CCC != "MMM" & CCC != "Sum", newfreq := Nccc - MISccc]
    ddd1[RRR == "Sum" & CCC == "Sum", newfreq := Freq - MISccc - MISrrr + misinboth]
    
    NN <- ddd1[RRR == "Sum" & CCC == "Sum", newfreq]
    if (length(NN) == 0 || NN == 0) NN <- 1 # Prevent divide by zero error
    
    # Create the display value: Freq (Percentage)
    ddd1[, value := data.table::fifelse(
      CCC == "MMM" | RRR == "MMM",
      base::as.character(Freq),
      base::paste0(newfreq, " (", ndformat(newfreq / NN * 100, ndp), "%)")
    )]
  }
  
  # 4. Pivot to Wide Format
  # Lock factors to ensure dcast doesn't alphabetically scramble the addmargins order
  ddd1[, CCC := factor(CCC, levels = lvls_ccc)]
  ddd1[, RRR := factor(RRR, levels = lvls_rrr)]
  
  out <- data.table::dcast(ddd1, RRR ~ CCC, value.var = "value", drop = FALSE)
  out[, RRR := base::as.character(RRR)]
  
  # 5. Add a placeholder row for the column variable label
  # Extract first row, replace all with NA, assign row name, and rbind
  top_row <- out[1, ]
  for (j in names(top_row)) {
    data.table::set(top_row, j = j, value = NA_character_)
  }
  top_row[, RRR := row] # Use the row variable name here
  
  out <- rbind(top_row, out)
  
  # 6. Perform Statistical Test (on non-missing data)
  tmp_test <- tmpDat[CCC != "MMM" & RRR != "MMM"]
  tmp_test[, CCC := base::as.character(CCC)]
  tmp_test[, RRR := base::as.character(RRR)]
  
  if (nrow(tmp_test) > 0 && length(unique(tmp_test$CCC)) > 1 && length(unique(tmp_test$RRR)) > 1) {
    tabla <- base::table(tmp_test$CCC, tmp_test$RRR)
    chi_test <- base::suppressWarnings(stats::chisq.test(tabla, correct = FALSE))
    
    if (base::all(chi_test$expected >= 5)) {
      p_val <- chi_test$p.value
      test_used <- "Chi-square test"
    } else {
      fisher_test <- tryCatch(stats::fisher.test(tabla), error = function(e) NULL)
      if (!is.null(fisher_test)) {
        p_val <- fisher_test$p.value
        test_used <- "Fisher's exact test"
      } else {
        p_val <- NA
        test_used <- "Fisher's exact test (failed)"
      }
    }
  } else {
    p_val <- NA
    test_used <- "Not enough valid data"
  }
  
  # 7. Add test results and return
  out[, test_name := test_used]
  out[, p.value := p_val]
  
  return(out)
}