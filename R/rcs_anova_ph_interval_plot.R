#' @title Generate a Forest Plot of Pairwise Comparison Results
#'
#' @description This function performs pairwise comparisons (either Welch's t-tests or Tukey's HSD)
#' and generates a forest plot to visualize the point estimates and confidence
#' intervals of the differences between groups using base R and `data.table` (no `rstatix` or `dplyr`).
#'
#' @param data A data frame containing the variables.
#' @param variable A character string specifying the name of the dependent variable (the continuous variable).
#' @param by A character string specifying the name of the grouping variable (the categorical variable).
#' @param type A character string specifying the type of post-hoc test to perform and the p-value adjustment method.
#'   Valid options are:
#'   \itemize{
#'     \item `"none"`: No p-value adjustment.
#'     \item `"holm"`: Holm (1979) method.
#'     \item `"hochberg"`: Hochberg (1988) method.
#'     \item `"hommel"`: Hommel (1988) method.
#'     \item `"bonferroni"`: Bonferroni correction.
#'     \item `"tukey"`: Tukey's Honest Significant Difference test (Tukey, 1949).
#'   }
#'
#' @return A `ggplot` object representing the forest plot of the pairwise comparisons.
#'
#' @references
#' \itemize{
#'   \item Holm, S. (1979). A simple sequentially rejective multiple test procedure. \emph{Scandinavian Journal of Statistics}, 6, 65–70.
#'   \item Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. \emph{Biometrika}, 75, 800–803.
#'   \item Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. \emph{Biometrika}, 75, 383–386.
#'   \item Tukey, J. W. (1949). The problem of multiple comparisons. Unpublished manuscript.
#' }
#'
#' @examples
#' # Create a sample data frame
#' data_df <- data.frame(
#'   len = c(4.2, 11.5, 7.3, 5.8, 6.4, 10, 11.2, 11.2, 5.2, 7,
#'           16.5, 16.5, 15.2, 17.3, 22.5, 17.3, 13.6, 14.5, 18.8, 15.5,
#'           23.6, 18.5, 33.9, 25.5, 26.4, 32.5, 26.7, 21.5, 23.3, 29.5),
#'   dose = factor(rep(c("Low", "Medium", "High"), each = 10))
#' )
#'
#' # Generate a forest plot using Bonferroni correction
#' plot1 <- rcs_anova_ph_interval_plot(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose",
#'   type = "bonferroni"
#' )
#' print(plot1)
#'
#' # Generate a forest plot using Tukey's HSD
#' plot2 <- rcs_anova_ph_interval_plot(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose",
#'   type = "tukey"
#' )
#' print(plot2)
#' @export
rcs_anova_ph_interval_plot <- function(data, variable, by,  type) {
  
  cole <- 0.95 
  
  # Ensure input is a data.table
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
  #out <- out[.N:1]
  
  # Create the forest plot
  plot_intervals <- ggplot2::ggplot(out, ggplot2::aes(x = estimate, y = contrast)) +
    # Add horizontal lines for confidence intervals
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low, xmax = conf.high), 
                            height = 0.2, linewidth = 1.2) +
    # Add points for the estimate
    ggplot2::geom_point(size = 3) +
    # Add a vertical reference line at 0
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "blue", linewidth = 2) +
    ggplot2::scale_y_discrete(limits = rev(out$contrast)) +
    # Define labels and titles with large font size
    ggplot2::labs(
      title = "Interval Plot of Group Differences",
      subtitle = paste("Confidence intervals (method = ", typelab, ")", sep = ""),
      x = "Difference (Point Estimate)",
      y = "Contrast"
    ) +
    # Customize the theme to increase font sizes
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 22, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 18),
      axis.title = ggplot2::element_text(size = 20),
      axis.text = ggplot2::element_text(size = 22),
      legend.title = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 16)
    )
  
  return(plot_intervals)
}