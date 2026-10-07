#' @title Perform a Two-Proportion Bootstrap Test and Generate an HTML Table
#' @description This function performs a two-proportion bootstrap analysis without relying on the `infer` package 
#'   and formats the results into a styled HTML table using `kableExtra`.
#'   It calculates a confidence interval and, optionally, a p-value, and also returns
#'   the plots for the null and bootstrap distributions.
#'
#' @param data A data frame containing the variables for the test.
#' @param variable A variable from the `data` frame to use as the response. Must be unquoted or string.
#' @param by A variable from the `data` frame to use as the explanatory variable. Must be unquoted or string.
#' @param conf_boot A numeric value specifying the confidence level for the interval (e.g., 0.95).
#' @param alt_boot A character string specifying the alternative hypothesis direction. Can be "two-sided", "two.sided", "greater", or "less".
#' @param nh_normal The hypothesized difference between the two proportions under the null hypothesis (default is `0`).
#' @param nd_num An integer specifying the number of decimal places for the confidence interval.
#' @param nd An integer specifying the number of decimal places (alternative alias to `nd_num`).
#' @param font_size An integer to set the font size of the table.
#' @param testyn_boot A logical value; if `TRUE`, a p-value column is included in the output table.
#' @param boot_nullplot_yn A logical value; if `TRUE`, generates and returns the null distribution plot.
#' @param boot_intervalplot_yn A logical value; if `TRUE`, generates and returns the bootstrap interval plot.
#'
#' @return A `list` containing three elements: `table`, which is a `kableExtra` HTML table object,
#'   `plot_null`, a ggplot object for the null distribution plot, and `plot_interval`, a ggplot
#'   object for the bootstrap confidence interval plot.
#'
#' @examples
#' # Create a dummy data frame
#' dummy_data <- data.frame(
#'   outcome = sample(factor(c("Yes", "No", "Yes", "No", "Yes", "Yes", "No", "No", "Yes", "No")), size = 20, replace = TRUE),
#'   group = sample(factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B")), size = 20, replace = TRUE)
#' )
#'
#' # Run the function with sample parameters
#' results <- rcs_twoprop_boot(
#'   data = dummy_data,
#'   variable = "outcome",
#'   by = "group",
#'   conf_boot = 0.95,
#'   alt_boot = "two.sided",
#'   nd_num = 2,
#'   font_size = 14,
#'   testyn_boot = TRUE
#' )
#'
#' rcs_twoprop_boot(
#'   data = dummy_data,
#'   variable = "outcome",
#'   by = "group",
#'   conf_boot = 0.95,
#'   alt_boot = "two.sided",
#'   nd_num = 2,
#'   font_size = 14,
#'   testyn_boot = TRUE
#' ) 
#' 
#' rcs_twoprop_boot(
#'   data = dummy_data,
#'   variable = "outcome",
#'   by = "group",
#'   conf_boot = 0.95,
#'   alt_boot = "two.sided",
#'   nd_num = 2,
#'   font_size = 14,
#'   testyn_boot = TRUE
#' ) 
#'
#' @export
rcs_twoprop_boot <- function(data, variable, by, conf_boot, alt_boot,nh_normal = 0, nd_num = 3, font_size = 16, testyn_boot = FALSE,
                             boot_nullplot_yn = TRUE, boot_intervalplot_yn = TRUE) {
  
  # =========================================================================
  # 1. DATA PREPARATION AND SIMULATION WORKFLOW
  # =========================================================================
  raw_var <- data[[variable]]
  raw_by <- data[[by]]
  
  dt <- data.table::data.table(
    response = raw_var,
    group = raw_by
  )
  dt <- na.omit(dt)
  
  if (!is.factor(dt$group)) dt[, group := as.factor(group)]
  if (!is.factor(dt$response)) dt[, response := as.factor(response)]
  
  group_levels <- levels(dt$group)
  if (length(group_levels) != 2) {
    stop("The 'by' variable must have exactly 2 levels.")
  }
  success_level <- levels(dt$response)[1]
  dt[, success := (response == success_level)]
  
  g1 <- group_levels[1]
  g2 <- group_levels[2]
  
  dt1 <- dt[group == g1]
  dt2 <- dt[group == g2]
  n1 <- nrow(dt1)
  n2 <- nrow(dt2)
  
  obs_p1 <- mean(dt1$success)
  obs_p2 <- mean(dt2$success)
  d_hat <- obs_p1 - obs_p2
  
  # Generate bootstrap distribution of difference in proportions (1,000 reps)
  boot_stats <- replicate(1000, {
    s1 <- dt1[sample(.N, n1, replace = TRUE), success]
    s2 <- dt2[sample(.N, n2, replace = TRUE), success]
    mean(s1) - mean(s2)
  })
  boot_dist <- data.table::data.table(stat = boot_stats)
  
  # Generate null distribution centered at nh_normal (independence) via permutation
  all_success <- dt$success
  all_group <- dt$group
  
  null_stats <- replicate(1000, {
    shd <- sample(all_success)
    p1_null <- mean(shd[all_group == g1])
    p2_null <- mean(shd[all_group == g2])
    p1_null - p2_null
  })
  null_dist <- data.table::data.table(stat = null_stats + nh_normal)
  
  # =========================================================================
  # 2. P-VALUE AND CONFIDENCE INTERVAL CALCULATION
  # =========================================================================
  if (alt_boot == "greater") {
    pv <- mean(null_dist$stat >= d_hat)
  } else if (alt_boot == "less") {
    pv <- mean(null_dist$stat <= d_hat)
  } else { # "two-sided" or "two.sided"
    obs_diff <- abs(d_hat - nh_normal)
    null_diff <- abs(null_dist$stat - nh_normal)
    pv <- mean(null_diff >= obs_diff)
  }
  

  pvalue <- pvformat(pv)
  
  alpha <- 1 - conf_boot
  
  if (alt_boot == "greater") {
    ci_lower <- quantile(boot_dist$stat, probs = alpha)
    percentile_ci <- c(ci_lower, 1)
    percentile_ci_str <- paste0("(", ndformat(ci_lower * 100, nd_num), "%, 100.0%)")
  } else if (alt_boot == "less") {
    ci_upper <- quantile(boot_dist$stat, probs = 1 - alpha)
    percentile_ci <- c(-1, ci_upper)
    percentile_ci_str <- paste0("(-100.0%, ", ndformat(ci_upper * 100, nd_num), "%)")
  } else { # "two-sided"
    percentile_ci <- quantile(boot_dist$stat, probs = c(alpha / 2, 1 - alpha / 2))
    percentile_ci_str <- paste0("(", ndformat(percentile_ci[1] * 100, nd_num), "%, ", ndformat(percentile_ci[2] * 100, nd_num), "%)")
  }
  
  # Create ggplot visualizations conditionally
  # Create ggplot visualizations conditionally
  plot_null <- NULL
  if (boot_nullplot_yn) {
    plot_null <- ggplot2::ggplot(null_dist, ggplot2::aes(x = stat * 100)) +
      ggplot2::geom_histogram(bins = 15, fill = "gray80", color = "white") +
      ggplot2::geom_vline(xintercept = d_hat * 100, color = "red", linewidth = 1, linetype = "dashed") +
      ggplot2::scale_x_continuous(
        #limits = c(-100, 100),
        labels = function(x) paste0(x, "%")
      ) +
      ggplot2::labs(title = "Null Distribution", x = "Statistic (%)", y = "Count") +
      ggplot2::theme_minimal()
  }
  
  plot_interval <- NULL
  if (boot_intervalplot_yn) {
    plot_interval <- ggplot2::ggplot(boot_dist, ggplot2::aes(x = stat * 100)) +
      ggplot2::geom_histogram(bins = 15, fill = "gray30", color = "white") +
      ggplot2::annotate(
        "rect", 
        xmin = percentile_ci[1] * 100, 
        xmax = percentile_ci[2] * 100, 
        ymin = 0, 
        ymax = Inf, 
        fill = "#56D6B5", 
        alpha = 0.5
      ) +
      ggplot2::scale_x_continuous(
        #limits = c(-100, 100),
        labels = function(x) paste0(x, "%")
      ) +
      ggplot2::labs(
        title = "Simulation-Based Bootstrap Distribution",
        x = "Statistic (%)",
        y = "count"
      ) +
      ggplot2::theme_minimal()
    
    ci_lower_pct <- percentile_ci[1] * 100
    ci_upper_pct <- percentile_ci[2] * 100
    
    if (is.finite(ci_lower_pct) && ci_lower_pct > -100) {
      plot_interval <- plot_interval + ggplot2::geom_vline(xintercept = ci_lower_pct, color = "#2ECC71", linewidth = 1)
    }
    if (is.finite(ci_upper_pct) && ci_upper_pct < 100) {
      plot_interval <- plot_interval + ggplot2::geom_vline(xintercept = ci_upper_pct, color = "#2ECC71", linewidth = 1)
    }
  }
  
  # =========================================================================
  # 3. BUILD THE TABLE DATA AND HTML STRINGS
  # =========================================================================
  dframe <- data.frame(
    Variable = success_level,
    diff = paste0(ndformat(mean(boot_stats) * 100, nd_num) ,"%"),
    Ci = percentile_ci_str,
    Pval = pvalue
  )
  row.names(dframe) <- NULL
  
  col_headers_html <- c(variable,"Bootstap Diff",
    paste(conf_boot * 100, "% Bootstrap CI for p<sub>1</sub> - p<sub>2</sub><sup>1</sup>", sep = ""),
    "P-value<sup>2</sup>"
  )
  
  if (!testyn_boot) {
    dframe <- dframe[, c("Variable","diff","Ci"), drop = FALSE]
    col_headers_html <- col_headers_html[1:3]
  }
  
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Bootstrap inference</p>",
    sep = ""
  )
  
  fn1 <- "<i>Based on percentiles</i>"
  if (testyn_boot) {
    fn2_body <- data.table::fcase(
      alt_boot == "greater",   paste0("H<sub>1</sub>: p<sub>1</sub> - p<sub>2</sub>&gt; ", nh_normal),
      alt_boot == "less",      paste0("H<sub>1</sub>: p<sub>1</sub> - p<sub>2</sub>&lt; ", nh_normal),
      alt_boot == "two-sided" | alt_boot == "two.sided", paste0("H<sub>1</sub>: p<sub>1</sub> - p<sub>2</sub>&ne; ", nh_normal)
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
    )|> 
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

