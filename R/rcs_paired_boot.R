#' @title Perform a Paired T-Test using Bootstrap Inference and Generate an HTML Table
#' @description This function performs a paired t-test using bootstrap resampling and
#'   permutation methods from the `infer` package. It calculates a confidence
#'   interval and a p-value, and then formats the results into a styled HTML table.
#'
#' @details This function uses the `infer` package's workflow to perform statistical
#'   inference. It generates a bootstrap distribution of the mean difference to
#'   calculate a percentile-based confidence interval. It also generates a null
#'   distribution via permutation (shuffling) to calculate the p-value for the
#'   specified null hypothesis.
#'
#'   The function returns a list containing the final HTML table and two `ggplot`
#'   objects, which visualize the null and bootstrap distributions.
#'
#' @param var1 A numeric vector representing the first variable.
#' @param var2 A numeric vector representing the second variable.
#' @param conf_boot A numeric value specifying the confidence level for the interval (e.g., 0.95).
#' @param alt_boot A character string specifying the alternative hypothesis direction.
#'   Can be "two_sided", "greater", or "less". Note the use of an underscore for "two_sided".
#' @param nd_num An integer specifying the number of decimal places for the output.
#' @param font_size An integer to set the font size of the table.
#' @param testyn_boot A logical value; if `TRUE`, the P-value column is included in the output table.
#'
#' @return A `list` with three elements:
#'   - `table`: a `kableExtra` HTML table object summarizing the results.
#'   - `plot_null`: a `ggplot` object visualizing the null distribution.
#'   - `plot_interval`: a `ggplot` object visualizing the bootstrap confidence interval.
#'
#' @examples
#' # Create example data
#' data_paired <- data.frame(
#'   before = c(10, 12, 15, 14, 18),
#'   after = c(15, 13, 16, 17, 20)
#' )
#'
#' # Example 1: Basic usage with a two-sided test and 95% confidence
#' boot_results <- rcs_paired_boot(
#'   var1 = data_paired$before,
#'   var2 = data_paired$after,
#'   conf_boot = 0.95,
#'   alt_boot = "two-sided",
#'   nd_num = 3
#' )
#'
#' # Accessing the results
#' boot_results$table
#'
#' # Example 2: Exclude the p-value column
#' rcs_paired_boot(
#'   var1 = data_paired$before,
#'   var2 = data_paired$after,
#'   conf_boot = 0.95,
#'   alt_boot = "two-sided",
#'   nd_num = 3,
#'   testyn_boot = TRUE
#' )$table
#'
#' # Example 3: One-sided test with a different null hypothesis
#' rcs_paired_boot(
#'   var1 = data_paired$before,
#'   var2 = data_paired$after,
#'   conf_boot = 0.90,
#'   alt_boot = "less",
#'   nd_num = 3
#' )$table
#' 
#' 

rcs_paired_boot <- function(var1, var2, conf_boot, alt_boot, nd_num, font_size = 16, testyn_boot = FALSE) {
  
  # =========================================================================
  # 1. DATA PREPARATION AND BOOTSTRAP RESAMPLING
  # =========================================================================
  # Extract paired non-missing differences
  diff_vec <- na.omit(var1 - var2)
  n <- length(diff_vec)
  
  if (n < 2) {
    stop("Insufficient non-missing paired data to perform bootstrap inference.")
  }
  
  # Calculate observed mean difference
  x_tilde <- mean(diff_vec)
  
  # Generate bootstrap distribution of sample mean differences (1,000 reps)
  boot_dist <- data.table::data.table(
    stat = replicate(1000, mean(sample(diff_vec, size = n, replace = TRUE)))
  )
  x_tilde_boot <- mean(boot_dist$stat)
  
  
  # Generate null distribution centered at 0
  null_dist <- data.table::data.table(
    stat = boot_dist$stat - x_tilde
  )
  
  # =========================================================================
  # 2. P-VALUE AND DIRECTIONAL CONFIDENCE INTERVAL CALCULATION
  # =========================================================================
  # Standardize alternative hypothesis parameter string
  alt_clean <- gsub("_", "-", tolower(alt_boot))
  
  # Calculate p-value based on direction
  if (alt_clean == "greater") {
    pv <- mean(null_dist$stat >= x_tilde)
  } else if (alt_clean == "less") {
    pv <- mean(null_dist$stat <= x_tilde)
  } else { # "two-sided" / "two_sided"
    pv <- mean(abs(null_dist$stat) >= abs(x_tilde))
  }
  
  pvalue <- pvformat(pv)
  
  # Calculate directional percentile confidence interval
  alpha <- 1 - conf_boot
  
  if (alt_clean == "greater") {
    ci_lower <- quantile(boot_dist$stat, probs = alpha)
    percentile_ci <- c(ci_lower, Inf)
    percentile_ci_str <- paste0("(", ndformat(ci_lower, nd_num), ", Inf)")
  } else if (alt_clean == "less") {
    ci_upper <- quantile(boot_dist$stat, probs = 1 - alpha)
    percentile_ci <- c(-Inf, ci_upper)
    percentile_ci_str <- paste0("(-Inf, ", ndformat(ci_upper, nd_num), ")")
  } else { # two-sided
    percentile_ci <- quantile(boot_dist$stat, probs = c(alpha / 2, 1 - alpha / 2))
    percentile_ci_str <- paste0("(", paste(ndformat(percentile_ci, nd_num), collapse = ", "), ")")
  }
  
  # =========================================================================
  # 3. GGPLOT VISUALIZATIONS
  # =========================================================================
  # Null Distribution Plot
  plot_null <- ggplot2::ggplot(null_dist, ggplot2::aes(x = stat)) +
    ggplot2::geom_histogram(bins = 30, fill = "gray80", color = "white") +
    ggplot2::geom_vline(xintercept = x_tilde, color = "red", linewidth = 1, linetype = "dashed") +
    ggplot2::labs(title = "Null Distribution", x = "Statistic", y = "Count") +
    ggplot2::theme_minimal()
  
  # Bootstrap Confidence Interval Plot
  plot_interval <- ggplot2::ggplot(boot_dist, ggplot2::aes(x = stat)) +
    ggplot2::geom_histogram(bins = 15, fill = "gray30", color = "white") +
    ggplot2::annotate(
      "rect", 
      xmin = percentile_ci[1], 
      xmax = percentile_ci[2], 
      ymin = 0, 
      ymax = Inf, 
      fill = "#56D6B5", 
      alpha = 0.5
    ) +
    ggplot2::labs(
      title = "Simulation-Based Bootstrap Distribution",
      x = "stat",
      y = "count"
    ) +
    ggplot2::theme_minimal()
  
  # Add vertical boundary lines for finite interval limits
  if (is.finite(percentile_ci[1])) {
    plot_interval <- plot_interval + ggplot2::geom_vline(xintercept = percentile_ci[1], color = "#2ECC71", linewidth = 1)
  }
  if (is.finite(percentile_ci[2])) {
    plot_interval <- plot_interval + ggplot2::geom_vline(xintercept = percentile_ci[2], color = "#2ECC71", linewidth = 1)
  }
  
  # =========================================================================
  # 4. BUILD TABLE DATA AND HTML STRINGS
  # =========================================================================
  dframe <- data.table::data.table(
    md   = ndformat(x_tilde_boot, nd_num),
    Ci   = percentile_ci_str,
    Pval = pvalue
  )
  
  col_headers_html <- c(
    "Mean Diff",
    paste0(conf_boot * 100, "% Bootstrap CI for d<sup>1</sup>"),
    "P-value<sup>2</sup>"
  )
  
  # Conditionally drop P-value column if not requested
  if (!testyn_boot) {
    dframe[, Pval := NULL]
    col_headers_html <- col_headers_html[col_headers_html != "P-value<sup>2</sup>"]
  }
  
  caption_html <- paste0(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Bootstrap inference</p>"
  )
  
  fn1 <- "<i>Based on percentiles</i>"
  
  if (testyn_boot) {
    fn2_body <- data.table::fcase(
      alt_clean == "greater", "H<sub>1</sub>: d &gt; 0",
      alt_clean == "less",    "H<sub>1</sub>: d &lt; 0",
      default =               "H<sub>1</sub>: d &ne; 0"
    )
    fn2 <- paste0("<i>", fn2_body, "</i>")
    footnotes_html <- c(fn1, fn2)
  } else {
    footnotes_html <- fn1
  }
  
  # =========================================================================
  # 5. BUILD KABLEEXTRA TABLE
  # =========================================================================
  table_out <- knitr::kable(
    dframe,
    format = "html",
    align = "c",
    col.names = col_headers_html,
    caption = caption_html,
    escape = FALSE
  ) |> 
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    ) |> 
    kableExtra::column_spec(
      column = 1:ncol(dframe),
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
      extra_css = "border-bottom: 2px solid #666; border-top: 1px solid #ddd;"
    ) |> 
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  list(table = table_out, plot_null = plot_null, plot_interval = plot_interval)
}