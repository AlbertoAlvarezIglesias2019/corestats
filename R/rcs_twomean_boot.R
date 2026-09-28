#' @title Two-Sample Bootstrap Inference
#' @description This function performs bootstrap analysis to calculate a
#'   confidence interval for the difference in means between two groups and a
#'   p-value for a permutation test. It is designed to be a robust alternative
#'   to a standard t-test, particularly when assumptions of normality are
#'   violated. 
#'
#' @param data A data frame or data.table containing the variables for the analysis.
#' @param variable A character string specifying the name of the numeric
#'   variable of interest.
#' @param by A character string specifying the name of the grouping factor
#'   variable. This factor must have exactly two levels.
#' @param conf_boot A numeric value between 0 and 1 specifying the confidence
#'   level for the bootstrap confidence interval.
#' @param nh_boot A numeric value representing the null hypothesis mean difference.
#'   Defaults to 0.
#' @param alt_boot A character string specifying the alternative hypothesis,
#'   must be one of "two-sided", "greater", or "less".
#' @param nd An integer specifying the number of decimal places for rounding
#'   the results.
#' @param font_size A numeric value for the font size of the output table.
#' @param testyn_boot A logical value; if `TRUE`, a p-value and the
#'   corresponding alternative hypothesis footnote are included in the table.
#'   Defaults to `FALSE`.
#'
#' @return A list containing three elements:
#' \item{table}{A `kableExtra` object representing the formatted results table.}
#' \item{plot_null}{A `ggplot` object visualizing the null distribution and
#'   the observed statistic.}
#' \item{plot_interval}{A `ggplot` object visualizing the bootstrap distribution
#'   and the percentile confidence interval.}
#'
#' @import data.table
#' @importFrom kableExtra kable_styling column_spec row_spec footnote
#' @importFrom knitr kable
#' @importFrom stats na.omit quantile
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline labs theme_minimal annotate
#'
#' @examples
#' set.seed(42)
#' my_data <- data.frame(
#'   Response = c(rnorm(50, 10, 2), rnorm(50, 12, 2)),
#'   Group = factor(c(rep("Group 1", 50), rep("Group 2", 50)))
#' )
#'
#' boot_results <- rcs_twomean_boot(
#'   data = my_data,
#'   variable = "Response",
#'   by = "Group",
#'   conf_boot = 0.95,
#'   nh_boot = 0,
#'   alt_boot = "two-sided",
#'   nd_num = 3,
#'   font_size = 12,
#'   testyn_boot = TRUE
#' )
#'
#' boot_results$table

rcs_twomean_boot <- function(data, variable, by, conf_boot, nh_boot, alt_boot,
                             nd_num, font_size, testyn_boot = FALSE,boot_nullplot_yn=FALSE, boot_intervalplot_yn=FALSE) {
  
  # =========================================================================
  # 1. DATA PREPARATION AND MANUAL SIMULATION
  # =========================================================================
  
  # Convert to data.table, isolate columns, and remove NAs
  dt <- data.table::as.data.table(data)[, c(variable, by), with = FALSE]
  data.table::setnames(dt, c("rrr", "ggg"))
  dt <- stats::na.omit(dt)
  
  # Extract group levels and split data
  group_levels <- levels(as.factor(dt$ggg))
  l1 <- group_levels[1]
  l2 <- group_levels[2]
  
  x1 <- dt$rrr[dt$ggg == l1]
  x2 <- dt$rrr[dt$ggg == l2]
  n1 <- length(x1)
  n2 <- length(x2)
  n_total <- n1 + n2
  x_all <- dt$rrr
  
  # 1. Calculate observed sample mean difference (l1 - l2)
  d_hat <- mean(x1) - mean(x2)
  
  # 2. Generate bootstrap distribution of sample mean differences
  # Resample WITH replacement within each group independently
  boot_dist <- data.table::data.table(
    stat = replicate(1000, mean(sample(x1, size = n1, replace = TRUE)) - 
                       mean(sample(x2, size = n2, replace = TRUE)))
  )
  d_hat_boot <- mean(boot_dist$stat)
  
  # 3. Generate null distribution (permutation test)
  # Shuffle group labels WITHOUT replacement to simulate independence
  null_dist <- data.table::data.table(
    stat = replicate(1000, {
      shuffled <- sample(x_all, size = n_total, replace = FALSE)
      # First n1 elements become group 1, remaining n2 become group 2
      mean(shuffled[1:n1]) - mean(shuffled[(n1 + 1):n_total])
    }) + nh_boot # Shift center to null hypothesis value
  )
  
  # =========================================================================
  # 2. P-VALUE AND CONFIDENCE INTERVAL CALCULATION
  # =========================================================================
  
  # 1. Calculate p-value based on direction
  if (alt_boot == "greater") {
    pv <- mean(null_dist$stat >= d_hat)
  } else if (alt_boot == "less") {
    pv <- mean(null_dist$stat <= d_hat)
  } else {
    obs_diff <- abs(d_hat - nh_boot)
    null_diff <- abs(null_dist$stat - nh_boot)
    pv <- mean(null_diff >= obs_diff)
  }
  
  # Format the p-value
  pvalue <- pvformat(pv)
  
  # 2. Calculate directional percentile confidence interval
  alpha <- 1 - conf_boot
  
  if (alt_boot == "greater") {
    ci_lower <- stats::quantile(boot_dist$stat, probs = alpha)
    percentile_ci <- c(ci_lower, Inf)
    percentile_ci_str <- paste0("(", ndformat(ci_lower, nd_num), ", Inf)")
  } else if (alt_boot == "less") {
    ci_upper <- stats::quantile(boot_dist$stat, probs = 1 - alpha)
    percentile_ci <- c(-Inf, ci_upper)
    percentile_ci_str <- paste0("(-Inf, ", ndformat(ci_upper, nd_num), ")")
  } else {
    percentile_ci <- stats::quantile(boot_dist$stat, probs = c(alpha / 2, 1 - alpha / 2))
    percentile_ci_str <- paste0("(", paste(ndformat(percentile_ci, nd_num), collapse = ", "), ")")
  }
  
  # 3. Create ggplot equivalents
  if (boot_nullplot_yn) {
    plot_null <- ggplot2::ggplot(null_dist, ggplot2::aes(x = stat)) +
      ggplot2::geom_histogram(bins = 30, fill = "gray80", color = "white") +
      ggplot2::geom_vline(xintercept = d_hat, color = "red", linewidth = 1, linetype = "dashed") +
      ggplot2::labs(title = "Null Distribution", x = "Statistic", y = "Count") +
      ggplot2::theme_minimal()
  } else plot_null=NULL

  
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
        x = "Statistic",
        y = "Count"
      ) +
      ggplot2::theme_minimal()
    
    if (is.finite(percentile_ci[1])) {
      plot_interval <- plot_interval + ggplot2::geom_vline(xintercept = percentile_ci[1], color = "#2ECC71", linewidth = 1)
    }
    if (is.finite(percentile_ci[2])) {
      plot_interval <- plot_interval + ggplot2::geom_vline(xintercept = percentile_ci[2], color = "#2ECC71", linewidth = 1)
    }
  } else plot_interval <- NULL

  
  # =========================================================================
  # 3. BUILD THE TABLE DATA AND HTML STRINGS
  # =========================================================================
  
  dframe <- data.frame(
    Pe = ndformat(d_hat_boot, nd_num),
    Ci = percentile_ci_str,
    Pval = pvalue,
    stringsAsFactors = FALSE
  )
  
  col_headers_html <- c(
    "Bootstrap<br> Diff in Means",
    paste0(conf_boot * 100, "% Bootstrap CI for &mu;<sub>1</sub> - &mu;<sub>2</sub><sup>1</sup>"),
    "P-value<sup>2</sup>"
  )
  
  if (!testyn_boot) {
    dframe$Pval <- NULL
    col_headers_html <- col_headers_html[!col_headers_html %in% "P-value<sup>2</sup>"]
  }
  
  caption_html <- paste0(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Bootstrap inference</p>"
  )
  
  fn1 <- "<i>Based on percentiles</i>"
  
  if (testyn_boot) {
    fn2_body <- data.table::fcase(
      alt_boot == "greater",   paste0("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub>&gt;", nh_boot),
      alt_boot == "less",      paste0("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub>&lt;", nh_boot),
      alt_boot == "two-sided", paste0("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub>&ne;", nh_boot)
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
    # CSS vertical-align rules applied below to fix p-value misalignment
    kableExtra::column_spec(
      column = 1:ncol(dframe),
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px; vertical-align: middle;"
    ) |> 
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd; padding-left: 10px; padding-right: 10px; vertical-align: middle;"
    ) |> 
    kableExtra::row_spec(
      row = 1,
      extra_css = "border-bottom: 2px solid #666; border-top: 1px solid #ddd; vertical-align: middle;"
    ) |> 
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  list(table = table_out, plot_null = plot_null, plot_interval = plot_interval)
}