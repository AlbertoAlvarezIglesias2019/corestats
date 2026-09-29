#' @title Normality and Variance Assessment for Two Variables
#' @description Generates a 3-column graphical assessment matching the layout grid:
#'   Boxplots (Col 1), Q-Q Plots (Col 2), Shapiro-Wilk results (Col 3), and Levene's Test (Bottom).
#' @param x1 Numeric vector for group 1.
#' @param x2 Numeric vector for group 2.
#' @param var_name1 Character string for group 1 label. Defaults to "Control".
#' @param var_name2 Character string for group 2 label. Defaults to "Exercise".
#' @param font_scale Numeric scaling factor for text size. Defaults to 2.0.
#' @return Generates a multi-panel plot on the current graphics device.
#' @importFrom stats shapiro.test qqnorm qqline median lm anova
#' @importFrom graphics boxplot layout par plot.new plot.window text

assess_normality_2var <- function(x1, x2, var_name1 = "Control", var_name2 = "Exercise", font_scale = 2) {
  
  # --- Input Validation ---
  if (!is.numeric(x1) || !is.numeric(x2)) {
    stop("Inputs 'x1' and 'x2' must be numeric vectors.")
  }
  
  x1 <- as.numeric(x1[!is.na(x1)])
  x2 <- as.numeric(x2[!is.na(x2)])
  
  if (length(x1) < 3 || length(x2) < 3) {
    stop("Input vectors must each have at least 3 non-missing values.")
  }
  
  # --- Base R Helper for Levene's Test ---
  .levene_test_base <- function(v1, v2) {
    med1 <- stats::median(v1)
    med2 <- stats::median(v2)
    
    dev1 <- abs(v1 - med1)
    dev2 <- abs(v2 - med2)
    
    devs <- c(dev1, dev2)
    grp  <- factor(c(rep("G1", length(v1)), rep("G2", length(v2))))
    
    fit <- stats::lm(devs ~ grp)
    aov_res <- stats::anova(fit)
    
    f_stat <- aov_res["grp", "F value"]
    p_val  <- aov_res["grp", "Pr(>F)"]
    
    list(statistic = f_stat, p.value = p_val)
  }
  
  # --- Subsampling Guard (> 5000) ---
  .subsample_shapiro <- function(x) {
    if (length(x) > 5000) {
      warning("Shapiro-Wilk test limited to 5000 observations. Subsampling applied.")
      sample(x, 5000)
    } else {
      x
    }
  }
  
  # --- Preserving Graphical Parameters ---
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  
  # --- 3-Column Layout Matrix Definition ---
  graphics::layout(
    matrix(
      c(1, 1, 1,
        2, 3, 4,
        5, 6, 7,
        8, 8, 8),
      nrow = 4, ncol = 3, byrow = TRUE
    ),
    widths = c(2.2, 2.2, 1.8),
    heights = c(0.6, 2.5, 2.5, 0.6)
  )
  
  # --- Helper to Plot Group Row (Boxplot, Q-Q, and SW Text) ---
  .plot_group_row <- function(x, var_name) {
    shapiro_x <- .subsample_shapiro(x)
    shapiro_test <- stats::shapiro.test(shapiro_x)
    
    # 1. Boxplot (Light Blue fill, dark gray border & median line)
    graphics::par(mar = c(4, 4, 3, 1))
    graphics::boxplot(
      x, 
      horizontal = TRUE, 
      main = paste("Boxplot for", var_name), 
      col = "#bce0ee", 
      border = "#4a4a4a",
      medcol = "#333333",
      medlwd = 3,
      staplelty = 1,
      whisklty = 2,
      cex.main = 1.1 * font_scale,
      cex.axis = 0.9 * font_scale
    )
    
    # 2. Q-Q Plot
    graphics::par(mar = c(4, 4, 3, 1))
    stats::qqnorm(
      x, 
      main = paste("Normal Q-Q Plot for", var_name), 
      pch = 16, 
      col = "#666666",
      cex = 1.0 * font_scale,
      cex.main = 1.1 * font_scale,
      cex.lab = 0.9 * font_scale,
      cex.axis = 0.9 * font_scale
    )
    stats::qqline(x, col = "#4682b4", lwd = 2)
    
    # 3. Shapiro-Wilk Test Results Block (Column 3)
    graphics::par(mar = c(1, 1, 1, 1))
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    
    w_stat_str <- sprintf("%.4f", shapiro_test$statistic)
    p_val_str  <- if (shapiro_test$p.value < 0.001) "< 0.001" else sprintf("%.4f", shapiro_test$p.value)
    
    interp_str <- if (shapiro_test$p.value > 0.05) {
      "p > 0.05: Fail to reject H0\n(Data may be normal)"
    } else {
      "p <= 0.05: Reject H0\n(Data likely not normal)"
    }
    
    # Dynamic color for Shapiro-Wilk result
    sw_color <- if (shapiro_test$p.value > 0.05) "#15803d" else "#b91c1c"
    
    # Stacked text
    graphics::text(x = 0.05, y = 0.72, labels = "Shapiro-Wilk Test:", adj = 0, cex = 1.05 * font_scale, font = 2, col = "#000000")
    graphics::text(x = 0.05, y = 0.57, labels = paste("W =", w_stat_str), adj = 0, cex = 1.0 * font_scale, col = "#333333")
    graphics::text(x = 0.05, y = 0.44, labels = paste("p-value =", p_val_str), adj = 0, cex = 1.0 * font_scale, col = "#333333")
    graphics::text(x = 0.05, y = 0.22, labels = interp_str, adj = 0, cex = 1.0 * font_scale, font = 2, col = sw_color)
  }
  
  # --- PLOT 1: Top Title ---
  graphics::par(mar = c(0.5, 1, 1, 1))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::text(
    x = 0.5, y = 0.5, 
    labels = "Normality and Variance Assessment", 
    cex = 1.5 * font_scale, 
    font = 2, 
    col = "#000000"
  )
  
  # --- PLOTS 2-7: Group 1 & Group 2 Rows ---
  .plot_group_row(x1, var_name1)
  .plot_group_row(x2, var_name2)
  
  # --- PLOT 8: Levene's Test Summary Row (Bottom Centered) ---
  graphics::par(mar = c(0.5, 1, 0.5, 1))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  
  levene_test <- .levene_test_base(x1, x2)
  levene_f <- levene_test$statistic
  levene_p <- levene_test$p.value
  
  levene_p_str <- if (levene_p < 0.001) "< 0.001" else sprintf("%.3f", levene_p)
  
  interp_levene <- if (levene_p > 0.05) {
    "Variances may be equal (p > 0.05)."
  } else {
    "Variances likely not equal (p <= 0.05)."
  }
  
  # Dynamic color for Levene's result
  levene_color <- if (levene_p > 0.05) "#15803d" else "#b91c1c"
  
  stats_prefix <- sprintf("Levene's Test for Homogeneity of Variance: F = %.3f, p = %s. ", levene_f, levene_p_str)
  
  # Draw prefix in standard dark text, then append colored interpretation
  prefix_width <- graphics::strwidth(stats_prefix, cex = 1.0 * font_scale)
  interp_width <- graphics::strwidth(interp_levene, cex = 1.0 * font_scale, font = 2)
  total_width  <- prefix_width + interp_width
  
  start_x <- 0.45 - (total_width / 2)
  
  graphics::text(x = start_x, y = 0.5, labels = stats_prefix, adj = 0, cex = 1.0 * font_scale, font = 2, col = "#000000")
  graphics::text(x = start_x + prefix_width+0.04, y = 0.5, labels = interp_levene, adj = 0, cex = 1.0 * font_scale, font = 2, col = levene_color)
}