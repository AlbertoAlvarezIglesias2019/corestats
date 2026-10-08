#' @title Generate a Boxplot with Pairwise Comparison Results
#'
#' @description This function performs pairwise comparisons (either Welch's t-tests or Tukey's HSD)
#' and generates a boxplot annotated with significance bars and p-value stars
#' to visualize post-hoc test results using base R and `data.table` (no `rstatix` or `dplyr`).
#'
#' @param data A data frame containing the variables.
#' @param variable A character string specifying the name of the dependent variable (the continuous variable).
#' @param by A character string specifying the name of the grouping variable (the categorical variable).
#' @param type A character string specifying the type of post-hoc test to perform and the p-value adjustment method.
#'   Valid options are:
#'   \itemize{
#'     \item `"none"`: No p-value adjustment.
#'     \item `"holm"`: Holm (1979) method.
#'     \item \code{"hochberg"}: Hochberg (1988) method.
#'     \item `"hommel"`: Hommel (1988) method.
#'     \item `"bonferroni"`: Bonferroni correction.
#'     \item `"tukey"`: Tukey's Honest Significant Difference test.
#'   }
#'
#' @return A `ggplot` object representing the annotated boxplot.
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
#' # Generate a boxplot with Bonferroni correction
#' plot1 <- rcs_anova_ph_inference_plot(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose",
#'   type = "bonferroni"
#' )
#' print(plot1)
#'
#' # Generate a boxplot with Tukey's HSD
#' plot2 <- rcs_anova_ph_inference_plot(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose",
#'   type = "tukey"
#' )
#' print(plot2)
#' @export
rcs_anova_ph_inference_plot <- function(data, variable, by, type) {
  
  # Ensure data is a data.table
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
      
      t_res <- stats::t.test(x1, x2, paired = FALSE, var.equal = FALSE)
      
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
  
  # Assign significance star categories
  fit[, p.adj.signif := data.table::fcase(
    p.adj >= 0.05, "ns",
    p.adj >= 0.01 & p.adj < 0.05, "*",
    p.adj >= 0.001 & p.adj < 0.01, "**",
    p.adj < 0.001, "***"
  )]
  
  # Compute y-positions for the annotation bars without rstatix
  max_val <- max(dt[[variable]], na.rm = TRUE)
  min_val <- min(dt[[variable]], na.rm = TRUE)
  y_range <- max_val - min_val
  if (y_range == 0) y_range <- abs(max_val)
  if (y_range == 0) y_range <- 1
  
  step <- y_range * 0.1
  fit[, y.position := max_val + seq_len(.N) * step]
  
  pwc <- fit
  max_y_pwc <- max(pwc$y.position, na.rm = TRUE)
  y_limits <- c(NA, max_y_pwc + step * 0.5)
  ytextbuffer <- step * 0.25
  
  by_col <- dt[[by]]
  by_levels <- if (is.factor(by_col)) levels(by_col) else unique(by_col)
  
  # Build the ggplot object
  plot_signif <- ggplot2::ggplot(data = dt, ggplot2::aes(x = factor(.data[[by]]), y = .data[[variable]])) +
    # A. Add the boxplot
    ggplot2::geom_boxplot() +
    # B. Add the horizontal significance bars
    ggplot2::geom_segment(
      data = pwc,
      ggplot2::aes(x = group1, xend = group2, y = y.position, yend = y.position),
      inherit.aes = FALSE,
      lineend = "butt",
      arrow = ggplot2::arrow(ends = "both", angle = 80, length = ggplot2::unit(.1, "cm"))
    ) +
    # C. Add the text labels (p-adj stars)
    ggplot2::geom_text(
      data = pwc,
      ggplot2::aes(
        x = (as.numeric(factor(group1, levels = by_levels)) + as.numeric(factor(group2, levels = by_levels))) / 2,
        y = y.position + ytextbuffer,
        label = p.adj.signif
      ),
      inherit.aes = FALSE,
      size = 4,
      fontface = "bold"
    ) +
    # D. Add titles and labels
    ggplot2::labs(
      title = "Boxplot with Post Hoc Test Results",
      subtitle = paste("Pairwise t-tests with ", typelab, " correction", sep = ""),
      x = by,
      y = variable
    ) +
    # E. Set the y-axis limits
    ggplot2::coord_cartesian(ylim = y_limits) +
    # F. Customize the theme for a cleaner look
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 19, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 15),
      axis.title = ggplot2::element_text(size = 17),
      axis.text = ggplot2::element_text(size = 19),
      legend.title = ggplot2::element_text(size = 15),
      legend.text = ggplot2::element_text(size = 13)
    )
  
  return(plot_signif)
}