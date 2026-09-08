assess_normality <- function(x, var_name = "Data", font_scale = 1.7) {
  
  # --- Input Validation ---
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector.")
  }
  
  x <- x[!is.na(x)]
  
  if (length(x) < 3) {
    stop("Input vector must have at least 3 non-missing values.")
  }
  
  # Shapiro-Wilk test limit check (base R shapiro.test handles N <= 5000)
  if (length(x) > 5000) {
    warning("Shapiro-Wilk test is limited to 5000 observations. Subsampling 5000 values.")
    shapiro_x <- sample(x, 5000)
  } else {
    shapiro_x <- x
  }
  
  # --- 1. Perform Shapiro-Wilk Normality Test ---
  shapiro_test <- stats::shapiro.test(shapiro_x)
  
  # --- Safely preserve and restore graphical parameters ---
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  # --- 2. Set up the plotting layout ---
  # 1: Title, 2: Boxplot, 3: QQ-Plot, 4: Test Results
  layout(
    matrix(c(1, 1, 2, 3, 4, 4), nrow = 3, ncol = 2, byrow = TRUE), 
    heights = c(0.6, 2, 0.9)
  )
  
  # --- PLOT 1: Main Title ---
  par(mar = c(0.5, 1, 2, 1))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  text(
    x = 0.5, y = 0.5, 
    labels = paste("Normality Assessment for", var_name), 
    cex = 1.8 * font_scale, 
    font = 2,
    col = "#1e293b"
  )
  
  # --- PLOT 2: Boxplot ---
  par(mar = c(4.5, 4.5, 3, 1.5))
  graphics::boxplot(
    x, 
    horizontal = TRUE, 
    main = "Boxplot", 
    xlab = var_name, 
    col = "#e2e8f0", 
    border = "#334155",
    cex.main = 1.4 * font_scale,
    cex.lab = 1.3 * font_scale,
    cex.axis = 1.2 * font_scale
  )
  
  # --- PLOT 3: QQ-Plot ---
  stats::qqnorm(
    x, 
    main = "Normal Q-Q Plot", 
    pch = 16, 
    col = "#334155",
    cex = 1.1 * font_scale,
    cex.main = 1.4 * font_scale,
    cex.lab = 1.3 * font_scale,
    cex.axis = 1.2 * font_scale
  )
  stats::qqline(x, col = "#0284c7", lwd = 2)
  
  # --- PLOT 4: Left-Aligned Test Results ---
  par(mar = c(0.5, 2, 0.5, 1))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  
  # Format statistics strings
  w_stat_str <- sprintf("%.4f", shapiro_test$statistic)
  p_val_str  <- if (shapiro_test$p.value < 0.001) "< 0.001" else sprintf("%.4f", shapiro_test$p.value)
  
  interp_str <- if (shapiro_test$p.value > 0.05) {
    "p > 0.05: Fail to reject H0 (Data may be normally distributed)"
  } else {
    "p <= 0.05: Reject H0 (Data significantly deviates from normal)"
  }
  
  # Left-aligned text output (adj = 0 aligns left edge at x = 0.05)
  text(x = 0, y = 0.70, labels = "Shapiro-Wilk Normality Test Results:", adj = 0, cex = 1.4 * font_scale, font = 2, col = "#1e293b")
  text(x = 0, y = 0.52, labels = paste0("W = ", w_stat_str, "   |   p-value = ", p_val_str), adj = 0, cex = 1.3 * font_scale, col = "#334155")
  text(x = 0, y = 0.35, labels = interp_str, adj = 0, cex = 1.3 * font_scale, font = 2, col = if (shapiro_test$p.value > 0.05) "#0f766e" else "#b91c1c")
}