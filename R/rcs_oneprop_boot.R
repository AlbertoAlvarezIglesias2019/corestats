#' @title Perform One-Proportion Bootstrap Inference
#' @description This function conducts a one-proportion bootstrap analysis to create a
#'   confidence interval and, optionally, a hypothesis test for a single proportion. It
#'   generates a confidence interval based on the percentile method and calculates a
#'   p-value using a simulated null distribution. The results are formatted into
#'   a customizable HTML table and optional plots of the distributions.
#'
#' @param data A data frame containing the variable to be analyzed.
#' @param variable A character string specifying the name of the categorical/binary variable
#'   in the data frame.
#' @param conf_boot A numeric value between 0 and 1 specifying the confidence level
#'   for the bootstrap confidence interval.
#' @param nh_boot The numeric value of the null hypothesis proportion (\eqn{p_0}).
#' @param alt_boot A character string specifying the alternative hypothesis
#'   direction. Must be one of `"greater"`, `"less"`, or `"two-sided"`.
#' @param nd_num The number of decimal places to round the output to.
#' @param font_size The font size for the output HTML table.
#' @param testyn_boot A logical value. If \code{TRUE}, a p-value for the
#'   hypothesis test is included in the output. If \code{FALSE} (the default),
#'   only the confidence interval is shown.
#' @param boot_nullplot_yn A logical value; if \code{TRUE}, generates and returns the null distribution plot.
#' @param boot_intervalplot_yn A logical value; if \code{TRUE}, generates and returns the bootstrap interval plot.
#' @return A list containing three elements:
#'   \item{table}{A \code{kableExtra} HTML table object showing the confidence interval and p-value.}
#'   \item{plot_null}{A \code{ggplot} object visualizing the null distribution (or \code{NULL} if disabled).}
#'   \item{plot_interval}{A \code{ggplot} object visualizing the confidence interval (or \code{NULL} if disabled).}
#'
#' @examples
#' # Create a sample dataset
#' library(kableExtra)
#' set.seed(123)
#' sample_data <- data.frame(
#'   response = factor(rbinom(100, 1, 0.4))
#' )
#'
#' # Example 1: Default usage with both plots enabled
#' rcs_oneprop_boot(
#'   data = sample_data,
#'   variable = "response",
#'   conf_boot = 0.95,
#'   nh_boot = 0.5,
#'   alt_boot = "two-sided",
#'   nd_num = 2,
#'   font_size = 12,
#'   testyn_boot = TRUE,
#'   boot_nullplot_yn = TRUE,
#'   boot_intervalplot_yn = TRUE
#' )$table
#'
rcs_oneprop_boot <- function(data, variable, conf_boot, nh_boot, alt_boot, nd_num, 
                             font_size = 16, testyn_boot = FALSE, 
                             boot_nullplot_yn = FALSE, boot_intervalplot_yn = FALSE) {
  
  # =========================================================================
  # 1. DATA PREPARATION AND SIMULATION WORKFLOW
  # =========================================================================
  raw_var <- data[[variable]]
  success_level <- levels(factor(raw_var))[1]
  x_clean <- na.omit(raw_var)
  n <- length(x_clean)
  
  success_mask <- (x_clean == success_level)
  x_successes <- sum(success_mask)
  p_hat <- x_successes / n
  
  # Generate bootstrap distribution of sample proportions (1,000 reps)
  boot_dist <- data.table::data.table(
    stat = replicate(1000, mean(sample(success_mask, size = n, replace = TRUE)))
  )
  p_hat_boot <- mean(boot_dist$stat)
  
  # Generate null distribution centered at nh_boot using binomial simulation
  null_dist <- data.table::data.table(
    stat = rbinom(1000, size = n, prob = nh_boot) / n
  )
  
  # =========================================================================
  # 2. P-VALUE AND CONFIDENCE INTERVAL CALCULATION
  # =========================================================================
  if (alt_boot == "greater") {
    pv <- mean(null_dist$stat >= p_hat)
  } else if (alt_boot == "less") {
    pv <- mean(null_dist$stat <= p_hat)
  } else { # "two-sided"
    obs_diff <- abs(p_hat - nh_boot)
    null_diff <- abs(null_dist$stat - nh_boot)
    pv <- mean(null_diff >= obs_diff)
  }
  
  pvalue <- pvformat(pv)
  
  # Calculate directional percentile confidence interval
  alpha <- 1 - conf_boot
  
  if (alt_boot == "greater") {
    ci_lower <- quantile(boot_dist$stat, probs = alpha)
    percentile_ci <- c(ci_lower, 1)
    percentile_ci_str <- paste0("(", ndformat(ci_lower * 100, nd_num), "%, 100.0%)")
  } else if (alt_boot == "less") {
    ci_upper <- quantile(boot_dist$stat, probs = 1 - alpha)
    percentile_ci <- c(0, ci_upper)
    percentile_ci_str <- paste0("(0.0%, ", ndformat(ci_upper * 100, nd_num), "%)")
  } else { # "two-sided"
    percentile_ci <- quantile(boot_dist$stat, probs = c(alpha / 2, 1 - alpha / 2))
    percentile_ci_str <- paste0("(", ndformat(percentile_ci[1] * 100, nd_num), "%, ", ndformat(percentile_ci[2] * 100, nd_num), "%)")
  }
  
  # Create ggplot visualizations conditionally
  plot_null <- NULL
  if (boot_nullplot_yn) {
    plot_null <- ggplot2::ggplot(null_dist, ggplot2::aes(x = stat)) +
      ggplot2::geom_histogram(bins = 30, fill = "gray80", color = "white") +
      ggplot2::geom_vline(xintercept = p_hat, color = "red", linewidth = 1, linetype = "dashed") +
      ggplot2::labs(title = "Null Distribution", x = "Statistic", y = "Count") +
      ggplot2::theme_minimal()
  }
  
  plot_interval <- NULL
  if (boot_intervalplot_yn) {
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
    
    if (is.finite(percentile_ci[1]) && percentile_ci[1] > 0) {
      plot_interval <- plot_interval + ggplot2::geom_vline(xintercept = percentile_ci[1], color = "#2ECC71", linewidth = 1)
    }
    if (is.finite(percentile_ci[2]) && percentile_ci[2] < 1) {
      plot_interval <- plot_interval + ggplot2::geom_vline(xintercept = percentile_ci[2], color = "#2ECC71", linewidth = 1)
    }
  }
  
  # =========================================================================
  # 3. BUILD THE TABLE DATA AND HTML STRINGS
  # =========================================================================
  dframe <- data.frame(
    Ps = paste0(ndformat(p_hat_boot * 100, nd_num), "%"),
    Ci = percentile_ci_str,
    Pval = pvalue
  )
  row.names(dframe) <- NULL
  
  col_headers_html <- c(
    paste0("Bootstrap ", success_level),
    paste0(conf_boot * 100, "% Bootstrap CI for p<sup>1</sup>"),
    "P-value<sup>2</sup>"
  )
  
  if (!testyn_boot) {
    dframe <- dframe[, c("Ps", "Ci")]
    col_headers_html <- col_headers_html[1:2]
  }
  
  caption_html <- paste0(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Bootstrap inference</p>"
  )
  
  fn1 <- "<i>Based on percentiles</i>"
  
  if (testyn_boot) {
    fn2_body <- data.table::fcase(
      alt_boot == "greater",   paste0("H<sub>1</sub>: p&gt;", nh_boot),
      alt_boot == "less",      paste0("H<sub>1</sub>: p&lt;", nh_boot),
      alt_boot == "two.sided", paste0("H<sub>1</sub>: p&ne;", nh_boot)
    )
    fn2 <- paste0("<i>", fn2_body, "</i>")
    footnotes_html <- c(fn1, fn2)
  } else {
    footnotes_html <- fn1
  }
  
  # =========================================================================
  # 4. BUILD THE KABLEEXTRA TABLE
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
    kableExtra::column_spec(
      column = 1,
      bold = TRUE
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