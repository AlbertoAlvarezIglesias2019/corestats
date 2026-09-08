#' Summarize Variables for Table 1 Generation (data.table Optimized)
#'
#' This function computes detailed summary statistics for continuous and categorical 
#' variables, optionally stratified by a grouping variable. It leverages \code{data.table} 
#' for maximal performance and memory efficiency. It calculates group-wise statistics, 
#' missing values, and appropriate p-values for comparisons across groups.
#'
#' @param data A data.frame or data.table containing the data.
#' @param variable A character vector specifying the name(s) of the variable(s) to summarize.
#' @param by A character string specifying the name of the grouping variable for stratification.
#'   Can be \code{NULL} for an overall summary.
#' @param suco A string specifying the format for \strong{continuous} statistics.
#'   Options supported: \code{"meansd"}, \code{"medianiqr"}, \code{"medianqs"}, or \code{"all"}.
#' @param suca A string specifying the format for \strong{categorical} statistics.
#'   Options supported: \code{"n"}, \code{"p"}, \code{"nNp"}, \code{"np"}, or \code{"pn"}.
#' @param nd_num An integer specifying the number of decimal places for numeric statistics.
#' @param nd_cat An integer specifying the number of decimal places for categorical percentages.
#'
#' @return A data.table containing summary statistics in a long format. Key columns 
#'   include \code{row_type}, \code{label}, \code{test_name}, \code{p.value}, 
#'   \code{variable}, \code{var_type}, \code{n}, and \code{stat_label}.
#'
#' @details This function determines whether a variable is continuous or categorical
#'   and applies the appropriate statistical summaries and hypothesis tests.
#'   \strong{Note:} This function expects an external formatting function 
#'   \code{ndformat()} to exist. It uses \code{shapiro.test} and 
#'   \code{car::leveneTest} for assumption checks for continuous variable testing.
#'
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' # Dummy ndformat if not present in your environment
#' ndformat <- function(x, nd) sprintf(paste0("%.", nd, "f"), x)
#'
#' data(iris)
#' library(data.table)
#' setDT(iris)
#' 
#' # Introduce some mock NA and Factor data
#' iris[Species == "virginica", Petal.Length := NA_real_]
#' iris[, Size := factor(fifelse(Sepal.Length > 5.8, "Large", "Small"))]
#'
#' # --- Example 1: Continuous variable (Sepal.Width) stratified by 'Species'
#' continuous_summary <- my_tbl_summary(
#'   data = iris,
#'   variable = "Sepal.Width",
#'   by = "Species",
#'   suco = "meansd",
#'   suca = "nNp",
#'   nd_num = 2,
#'   nd_cat = 1
#' )
#' print(continuous_summary)
#'
#' # --- Example 2: Categorical variable (Size) stratified by 'Species'
#' categorical_summary <- my_tbl_summary(
#'   data = iris,
#'   variable = "Size",
#'   by = "Species",
#'   suco = "meansd",
#'   suca = "nNp",
#'   nd_num = 1,
#'   nd_cat = 1
#' )
#' print(categorical_summary)
#' }
#'
#'
.datatable.aware <- TRUE

my_tbl_summary <- function(data, variable, by, suco, suca, nd_num, nd_cat) {
  
  # Ensure the input is a data.table without modifying the original object globally
  DT <- data.table::as.data.table(data)
  
  res_list <- lapply(variable, function(vvv) {
    
    # --- 1. Subsetting Data ---
    if (is.null(by)) {
      #tmp <- DT[, list(var_data = get(vvv))]
      tmp <- data.table::data.table(var_data = DT[[vvv]])
      by_col_exists <- FALSE
    } else {
      # Filter out records where grouping variable is missing
      #tmp <- DT[!is.na(get(by)), list(var_data = get(vvv), by_data = as.factor(get(by)))]
      tmp <- data.table::data.table(var_data = DT[[vvv]],by_data = as.factor(DT[[by]]))
      tmp <- tmp[!is.na(by_data)]
      by_col_exists <- TRUE
    }
    
    is_num <- is.numeric(tmp$var_data)
    n_valid_overall <- sum(!is.na(tmp$var_data))
    
    # -------------------------
    # --- A. CONTINUOUS DATA ---
    # -------------------------
    if (is_num) {
      
      # Helper function for numeric aggregation
      calc_stats <- function(d) {
        d[, list(
          n = .N,
          Mis = sum(is.na(var_data)),
          Mean = as.numeric(mean(var_data, na.rm = TRUE)),
          SD = as.numeric(stats::sd(var_data, na.rm = TRUE)),
          Median = as.numeric(stats::median(var_data, na.rm = TRUE)),
          Q1 = as.numeric(stats::quantile(var_data, 0.25, na.rm = TRUE)),
          Q3 = as.numeric(stats::quantile(var_data, 0.75, na.rm = TRUE)),
          Min = as.numeric(min(var_data, na.rm = TRUE)),
          Max = as.numeric(max(var_data, na.rm = TRUE))
        )]
      }
      
      overall_stats <- calc_stats(tmp)
      overall_stats[, by_data := if (by_col_exists) "Overall" else ""]
      
      if (by_col_exists) {
        grp_stats <- tmp[, calc_stats(.SD), by = by_data]
        stats_dt <- data.table::rbindlist(list(overall_stats, grp_stats), use.names = TRUE)
        stats_dt[, html_label := paste0(by_data, "<br> N = ", n)]
      } else {
        stats_dt <- overall_stats
        stats_dt[, html_label := paste0("N = ", n)]
      }
      
      # Extract missing row securely
      miss_row <- data.table::dcast(stats_dt, . ~ html_label, value.var = "Mis")
      miss_row[, `.` := NULL]
      miss_row[, `:=`(row_type = "missing", label = "(Unknown)")]
      
      # Apply Formatting layout based on 'suco'
      if (suco == "meansd") {
        stats_dt[, value := paste0(ndformat(Mean, nd_num), " (", ndformat(SD, nd_num), ")")]
        val_row <- data.table::dcast(stats_dt, . ~ html_label, value.var = "value")
        val_row[, `.` := NULL]
        val_row[, `:=`(row_type = "label", label = vvv, summary_label = "Mean (SD)")]
        
        out_df <- data.table::rbindlist(list(val_row, miss_row), fill = TRUE, use.names = TRUE)
        
      } else if (suco == "medianiqr") {
        stats_dt[, value := paste0(ndformat(Median, nd_num), " (", ndformat(Q3 - Q1, nd_num), ")")]
        val_row <- data.table::dcast(stats_dt, . ~ html_label, value.var = "value")
        val_row[, `.` := NULL]
        val_row[, `:=`(row_type = "label", label = vvv, summary_label = "Median (IQR)")]
        
        out_df <- data.table::rbindlist(list(val_row, miss_row), fill = TRUE, use.names = TRUE)
        
      } else if (suco == "medianqs") {
        stats_dt[, value := paste0(ndformat(Median, nd_num), " (", ndformat(Q1, nd_num), ",", ndformat(Q3, nd_num), ")")]
        val_row <- data.table::dcast(stats_dt, . ~ html_label, value.var = "value")
        val_row[, `.` := NULL]
        val_row[, `:=`(row_type = "label", label = vvv, summary_label = "Median (Q1,Q3)")]
        
        out_df <- data.table::rbindlist(list(val_row, miss_row), fill = TRUE, use.names = TRUE)
        
      } else if (suco == "all") {
        stats_dt[, `:=`(
          Mean = ndformat(Mean, nd_num), SD = ndformat(SD, nd_num), Median = ndformat(Median, nd_num),
          Q1 = ndformat(Q1, nd_num), Q3 = ndformat(Q3, nd_num), Min = ndformat(Min, nd_num), Max = ndformat(Max, nd_num)
        )]
        
        melted <- data.table::melt(stats_dt, id.vars = "html_label", measure.vars = c("Mean", "SD", "Median", "Q1", "Q3", "Min", "Max"))
        val_rows <- data.table::dcast(melted, variable ~ html_label, value.var = "value")
        data.table::setnames(val_rows, "variable", "label")
        val_rows[, `:=`(label = as.character(label), row_type = "level", summary_label = "")]
        
        lbl_row <- data.table::data.table(row_type = "label", label = vvv, summary_label = "")
        miss_row[, summary_label := ""]
        
        out_df <- data.table::rbindlist(list(lbl_row, val_rows, miss_row), fill = TRUE, use.names = TRUE)
      }
      
      # Statistical testing
      test_used <- NA_character_
      p_val <- NA_real_
      
      if (by_col_exists) {
        valid_tmp <- tmp[!is.na(var_data)]
        n_levels <- data.table::uniqueN(valid_tmp$by_data)
        
        if (n_levels >= 2) {
          grp_n <- valid_tmp[, .N, by = by_data]
          valid_grps <- grp_n[N >= 3, by_data]
          
          normal_met <- TRUE
          if (length(valid_grps) == n_levels) {
            for (g in valid_grps) {
              if (stats::shapiro.test(valid_tmp[by_data == g, var_data])$p.value < 0.05) {
                normal_met <- FALSE
                break
              }
            }
          } else {
            normal_met <- FALSE
          }
          
          homo_met <- TRUE
          if (normal_met && car::leveneTest(var_data ~ by_data, data = valid_tmp)$`Pr(>F)`[1] < 0.05) {
            homo_met <- FALSE
          }
          
          if (normal_met && homo_met) {
            if (n_levels == 2) {
              test_used <- "t-test"
              p_val <- stats::t.test(var_data ~ by_data, data = valid_tmp, var.equal = TRUE)$p.value
            } else {
              test_used <- "ANOVA"
              p_val <- summary(stats::aov(var_data ~ by_data, data = valid_tmp))[[1]][["Pr(>F)"]][1]
            }
          } else {
            if (n_levels == 2) {
              test_used <- "Wilcoxon rank-sum test"
              p_val <- stats::wilcox.test(var_data ~ by_data, data = valid_tmp)$p.value
            } else {
              test_used <- "Kruskal-Wallis test"
              p_val <- stats::kruskal.test(var_data ~ by_data, data = valid_tmp)$p.value
            }
          }
        }
      }
      
    } else {
      # --------------------------
      # --- B. CATEGORICAL DATA ---
      # --------------------------
      overall_counts <- tmp[, list(N = .N, Mis = sum(is.na(var_data)))]
      valid_tmp <- tmp[!is.na(var_data)]
      
      overall_levels <- valid_tmp[, list(n = .N), by = list(var_data)]
      overall_levels[, `:=`(by_data = if (by_col_exists) "Overall" else "",
                            N = overall_counts$N, Mis = overall_counts$Mis)]
      
      if (by_col_exists) {
        grp_counts <- tmp[, list(N = .N, Mis = sum(is.na(var_data))), by = by_data]
        grp_levels <- valid_tmp[, list(n = .N), by = list(by_data, var_data)]
        grp_levels <- merge(grp_levels, grp_counts, by = "by_data", all.x = TRUE)
        stats_dt <- data.table::rbindlist(list(overall_levels, grp_levels), use.names = TRUE, fill = TRUE)
        stats_dt[, html_label := paste0(by_data, "<br> N = ", N)]
      } else {
        stats_dt <- overall_levels
        stats_dt[, html_label := paste0("N = ", N)]
      }
      
      # Handle Missing row
      miss_row <- unique(stats_dt[, list(html_label, Mis)])
      miss_row <- data.table::dcast(miss_row, . ~ html_label, value.var = "Mis")
      miss_row[, `.` := NULL]
      miss_row[, `:=`(row_type = "missing", label = "(Unknown)")]
      
      # Ensure all factor levels (even those with 0 counts) are represented across groups (cross-join)
      all_levels <- if (is.factor(tmp$var_data)) levels(tmp$var_data) else unique(valid_tmp$var_data)
      all_labels <- unique(stats_dt$html_label)
      label_info <- unique(stats_dt[, list(html_label, N, Mis)])
      
      grid <- data.table::CJ(var_data = all_levels, html_label = all_labels)
      stats_dt <- merge(grid, stats_dt[, list(var_data, html_label, n)], by = c("var_data", "html_label"), all.x = TRUE)
      stats_dt[is.na(n), n := 0]
      stats_dt <- merge(stats_dt, label_info, by = "html_label", all.x = TRUE)
      
      # Formatting layout based on 'suca'
      if (suca == "n") {
        stats_dt[, value := as.character(n)]
        summary_lab <- "n"
      } else if (suca == "p") {
        stats_dt[, value := paste0(ndformat(n / (N - Mis) * 100, nd_cat), "%")]
        summary_lab <- "%"
      } else if (suca == "nNp") {
        stats_dt[, value := paste0(n, " / ", N - Mis, " (", ndformat(n / (N - Mis) * 100, nd_cat), "%)")]
        summary_lab <- "n / N (%)"
      } else if (suca == "np") {
        stats_dt[, value := paste0(n, " (", ndformat(n / (N - Mis) * 100, nd_cat), "%)")]
        summary_lab <- "n (%)"
      } else if (suca == "pn") {
        stats_dt[, value := paste0(ndformat(n / (N - Mis) * 100, nd_cat), "% (", n, ")")]
        summary_lab <- "% (n)"
      }
      
      val_rows <- data.table::dcast(stats_dt, var_data ~ html_label, value.var = "value")
      data.table::setnames(val_rows, "var_data", "label")
      val_rows[, `:=`(label = as.character(label), row_type = "level", summary_label = summary_lab)]
      
      lbl_row <- data.table::data.table(row_type = "label", label = vvv, summary_label = summary_lab)
      miss_row[, summary_label := summary_lab]
      
      out_df <- data.table::rbindlist(list(lbl_row, val_rows, miss_row), fill = TRUE, use.names = TRUE)
      
      # Statistical testing
      test_used <- NA_character_
      p_val <- NA_real_
      
      if (by_col_exists) {
        if (nrow(valid_tmp) > 0 && data.table::uniqueN(valid_tmp$by_data) >= 2) {
          tabla <- base::table(valid_tmp$var_data, valid_tmp$by_data)
          chi_test <- base::suppressWarnings(stats::chisq.test(tabla, correct = FALSE))
          
          if (base::all(chi_test$expected >= 5)) {
            p_val <- chi_test$p.value
            test_used <- "Chi-square test"
          } else {
            fisher_test <- tryCatch(stats::fisher.test(tabla), error = function(e) NULL)
            if (!is.null(fisher_test)) {
              p_val <- fisher_test$p.value
              test_used <- "Fisher's exact test"
            }
          }
        }
      }
    }
    
    # --------------------------
    # --- C. ATTACH METADATA ---
    # --------------------------
    out_df[, `:=`(
      test_name = test_used,
      p.value = p_val,
      variable = vvv,
      var_type = if (is_num) "continuous" else "categorical",
      n = n_valid_overall
    )]
    
    return(out_df)
  })
  
  # Combine results for all variables
  dframe <- data.table::rbindlist(res_list, fill = TRUE, use.names = TRUE)
  
  # Final label cleanup exactly mimicking original behavior 
  dframe[, stat_label := data.table::fifelse(row_type == "label", summary_label, NA_character_)]
  dframe[, summary_label := NULL]
  
  # Blank out n and p.value for all subset levels except the main label row
  dframe[row_type != "label", `:=`(n = NA_integer_, p.value = NA_real_)]
  
  return(dframe)
}