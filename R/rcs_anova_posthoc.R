#' @title Generate an HTML Table of Post-Hoc Pairwise Comparisons
#'
#' @description This function performs pairwise comparisons (t-tests or Tukey's HSD) and
#' formats the results into a well-styled, publication-quality HTML table using base R and `data.table`.
#'
#' @param data A data frame containing the variables.
#' @param variable A character string specifying the name of the dependent variable (the continuous variable).
#' @param by A character string specifying the name of the grouping variable (the categorical variable).
#' @param nd_num An integer specifying the number of decimal places to round the numeric results (estimates and confidence intervals). Defaults to 1.
#' @param ev A logical value. This parameter is currently unused and is included for internal purposes.
#' @param font_size A numeric value for the font size of the table.
#' @param miss_yn A logical value indicating whether missing values should be handled. Defaults to `FALSE`.
#' @param type A character string specifying the type of post-hoc test to perform and the p-value adjustment method.
#'   Valid options are:
#'   \itemize{
#'     \item `"none"`: No p-value adjustment.
#'     \item `"holm"`: Holm (1979) method.
#'     \item `"hochberg"`: Hochberg (1988) method.
#'     \item `"hommel"`: Hommel (1988) method.
#'     \item `"bonferroni"`: Bonferroni correction.
#'     \item `"tukey"`: Tukey's Honest Significant Difference test.
#'   }
#'
#' @return A character string containing the HTML code for the formatted table (`knitr::kable` object).
#'
#' @references
#' \itemize{
#'   \item Holm, S. (1979). A simple sequentially rejective multiple test procedure. \emph{Scandinavian Journal of Statistics}, 6, 65–70.
#'   \item Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. \emph{Biometrika}, 75, 800–803.
#'   \item Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. \emph{Biometrika}, 75, 383–386.
#' }
#'
#' @examples
#'
#' # Create a sample data frame
#' data_df <- data.frame(
#'   len = c(4.2, 11.5, 7.3, 5.8, 6.4, 10, 11.2, 11.2, 5.2, 7,
#'           16.5, 16.5, 15.2, 17.3, 22.5, 17.3, 13.6, 14.5, 18.8, 15.5,
#'           23.6, 18.5, 33.9, 25.5, 26.4, 32.5, 26.7, 21.5, 23.3, 29.5),
#'   dose = factor(rep(c("Low", "Medium", "High"), each = 10))
#' )
#'
#' # Generate a post-hoc table using Bonferroni correction
#' rcs_anova_posthoc(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose",
#'   nd_num = 2,
#'   font_size = 14,
#'   type = "bonferroni"
#' )
#' @export
rcs_anova_posthoc <- function(data, variable, by, nd_num = 1, ev = TRUE, font_size, miss_yn = FALSE, type) {
  
  cole <- 0.95 
  nd <- nd_num
  
  # --- 1. PREPARE DATA AND RUN COMPARISONS ---
  dt <- data.table::as.data.table(data)
  
  # Choose the label for the type
  typelab <- data.table::fcase(
    type == "none", "None",
    type == "holm", "Holm",
    type == "hochberg", "Hochberg",
    type == "hommel", "Hommel",
    type == "bonferroni", "Bonferroni",
    type == "tukey", "Tukey",
    default = "None"
  )
  
  formu <- as.formula(paste(variable, "~", by, sep = ""))
  
  if (type == "tukey") {
    fit_aov <- stats::aov(formu, data = dt)
    th <- stats::TukeyHSD(fit_aov)[[1]]
    pair_names <- rownames(th)
    split_names <- strsplit(pair_names, "-")
    
    fit_list <- lapply(seq_len(nrow(th)), function(i) {
      g2 <- split_names[[i]][1]
      g1 <- split_names[[i]][2]
      data.table::data.table(
        group1 = g1,
        group2 = g2,
        estimate = -th[i, "diff"],
        conf.low = -th[i, "upr"],
        conf.high = -th[i, "lwr"],
        p.adj = th[i, "p adj"],
        adjustmethod = "tukey"
      )
    })
    fit <- data.table::rbindlist(fit_list)
    
  } else {
    groups <- levels(factor(dt[[by]]))
    if (length(groups) < 2) {
      stop("The grouping variable must have at least 2 levels.")
    }
    pairs <- utils::combn(groups, 2, simplify = FALSE)
    
    res_list <- lapply(pairs, function(p) {
      g1 <- p[1]
      g2 <- p[2]
      
      x1 <- dt[get(by) == g1, get(variable)]
      x2 <- dt[get(by) == g2, get(variable)]
      
      t_res <- stats::t.test(x1, x2, paired = FALSE, var.equal = FALSE, conf.level = cole)
      
      data.table::data.table(
        group1 = g1,
        group2 = g2,
        estimate = mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE),
        conf.low = t_res$conf.int[1],
        conf.high = t_res$conf.int[2],
        p = t_res$p.value
      )
    })
    fit <- data.table::rbindlist(res_list)
    
    if (type != "none") {
      fit[, p.adj := stats::p.adjust(p, method = type)]
    } else {
      fit[, p.adj := p]
    }
    fit[, p := NULL]
  }
  
  out <- data.table::copy(fit)
  out[, contrast := paste(group1, group2, sep = " - ")]
  out <- out[, .(contrast, estimate, conf.low, conf.high, p.adj)]
  
  # Recalculate confidence intervals if Bonferroni correction is requested
  if (type == "bonferroni") {
    num_comparisons <- nrow(out)
    adj_conf_level <- 1 - (1 - cole) / num_comparisons
    
    groups <- levels(factor(dt[[by]]))
    pairs <- utils::combn(groups, 2, simplify = FALSE)
    
    bonf_list <- lapply(pairs, function(p) {
      g1 <- p[1]
      g2 <- p[2]
      x1 <- dt[get(by) == g1, get(variable)]
      x2 <- dt[get(by) == g2, get(variable)]
      t_res_bonf <- stats::t.test(x1, x2, paired = FALSE, var.equal = FALSE, conf.level = adj_conf_level)
      data.table::data.table(
        conf.low = t_res_bonf$conf.int[1],
        conf.high = t_res_bonf$conf.int[2]
      )
    })
    bonf_dt <- data.table::rbindlist(bonf_list)
    out$conf.low <- bonf_dt$conf.low
    out$conf.high <- bonf_dt$conf.high
  }
  
  # --- 2. FORMAT TABLE DATA ---
  dframe <- data.table::copy(out)
  dframe[, `:=`(
    estimate = ndformat(estimate, nd),
    conf.low = ndformat(conf.low, nd),
    conf.high = ndformat(conf.high, nd),
    p.adj = pvformat(p.adj)
  )]
  dframe[, Ci := paste("(", conf.low, ", ", conf.high, ")", sep = "")]
  dframe <- dframe[, .(contrast, estimate, Ci, p.adj)]
  dframe <- as.data.frame(dframe)
  
  col_headers_html <- c("Contrast", "Diff in Means", "95% adjusted CI<sup>1</sup>", "Adjusted P-value<sup>1</sup>")
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Post-Hoc Pairwise comparisons</p>",
    sep = ""
  )
  
  table_out_infe <- knitr::kable(
    dframe,
    format = "html",
    align = "lccc",
    col.names = col_headers_html,
    caption = caption_html,
    escape = FALSE
  ) |>
    # Style the table layout and font.
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    ) |>
    # Add vertical borders and padding to columns.
    kableExtra::column_spec(
      column = 1:dim(dframe)[2],
      border_left = "0.5px solid #ddd",
      border_right = "0.5px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    ) |>
    # Add horizontal borders to the header and bold the text.
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd; padding-left: 10px; padding-right: 10px;"
    ) |>
    # Add horizontal borders to the bottom row.
    kableExtra::row_spec(
      row = dim(dframe)[1],
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd;"
    ) |>
    # Add horizontal borders to data rows.
    kableExtra::row_spec(
      row = 1:dim(dframe)[1],
      extra_css = "border-top: 1px solid #ddd;"
    ) |>
    # Add footnotes to the table.
    kableExtra::footnote(
      number = paste("<i>", paste("Using a ", typelab, " correction", sep = ""), "</i>"),
      escape = FALSE
    )
  
  return(table_out_infe)
}