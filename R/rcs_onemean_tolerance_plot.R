#' @title Plot Normal Tolerance Intervals with ggplot2 and data.table
#'
#' @description Generates visual representations of normal tolerance limits using \code{ggplot2} 
#'   and \code{data.table}. Produces a histogram with a density curve, an individual 
#'   control/run chart, or both side-by-side. Designed for seamless rendering in 
#'   Jamovi modules without external tolerance package plot dependencies.
#'
#' @param tp A list containing plot parameters with the following elements:
#'   \describe{
#'     \item{\code{tol.out}}{A data frame containing calculated parameters (\code{alpha}, \code{P}, 
#'       \code{x.bar}, and tolerance bounds).}
#'     \item{\code{x}}{A numeric vector of raw observed values.}
#'     \item{\code{plot.type}}{A character string specifying the plot type: \code{"hist"} 
#'       (or \code{"histogram"}), \code{"control"} (or \code{"chart"}, \code{"run"}), or \code{"both"}.}
#'     \item{\code{y.lab}}{An optional character string specifying the variable label for axes.}
#'     \item{\code{font_size}}{An optional numeric value specifying the base font size (default is 14).}
#'   }
#'
#' @return A \code{\link[ggplot2]{ggplot}} object if a single plot is requested, or a 
#'   \code{gtable}/\code{gridExtra} layout object if \code{plot.type = "both"}.
#'
#' @import ggplot2
#' @importFrom data.table as.data.table := .N
#' @importFrom gridExtra grid.arrange
#'
#' @seealso \code{\link{rcs_onemean_tolerance}}
#'
#' @examples
#' # Generate sample dataset
#' set.seed(456)
#' sample_data <- data.frame(my_var = rnorm(100, mean = 50, sd = 10))
#' 
#' # Calculate tolerance interval parameters
#' tol_res <- rcs_onemean_tolerance(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_tolerance = 0.95,
#'   poco_tolerance = 0.95,
#'   side_tolerance = 2,
#'   plotype_tolerance = "both",
#'   nd_num = 2,
#'   font_size = 14
#' )
#'
#' # Render the plots
#' rcs_onemean_tolerance_plot(tol_res$plot)

rcs_onemean_tolerance_plot <- function(tp) {
  
  # --- 1. EXTRACT PARAMETERS & PREPARE DATA ---
  dt <- data.table::as.data.table(data.frame(val = tp$x))
  dt[, obs_id := seq_len(.N)]  # Clean sequence index without warnings
  
  fit <- tp$tol.out
  y_lab <- if (!is.null(tp$y.lab) && tp$y.lab != "") tp$y.lab else "X"
  plot_type <- tolower(tp$plot.type)
  
  # Base font size (uses tp$font_size if passed, otherwise defaults to 14)
  base_font <- if (!is.null(tp$font_size) && is.numeric(tp$font_size)) tp$font_size else 14
  
  # Extract tolerance bounds and mean
  x_bar <- fit$x.bar
  lower <- if ("2-sided.lower" %in% names(fit)) fit[["2-sided.lower"]] else fit[["1-sided.lower"]]
  upper <- if ("2-sided.upper" %in% names(fit)) fit[["2-sided.upper"]] else fit[["1-sided.upper"]]
  
  # Dynamic title string matching tolerance::plottol format
  conf_pct <- (1 - fit$alpha) * 100
  poco_pct <- fit$P * 100
  plot_title <- paste0(conf_pct, "% / ", poco_pct, "% Tolerance Limits")
  
  # Clean visual theme with enlarged typography
  theme_clean <- ggplot2::theme_minimal(base_size = base_font) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = base_font + 2, color = "#1e293b", hjust = 0.5),
      axis.title = ggplot2::element_text(face = "bold", size = base_font, color = "#475569"),
      axis.text = ggplot2::element_text(size = base_font - 1, color = "#334155")
    )
  
  # --- 2. DEFINE PLOT FUNCTIONS ---
  
  # Histogram Plot
  make_hist <- function() {
    p <- ggplot2::ggplot(dt, ggplot2::aes(x = val)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), fill = "#e2e8f0", color = "#94a3b8", bins = 20) +
      #ggplot2::geom_density(color = "#0284c7", linewidth = 0.9) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = x_bar), color = "#2563eb", linetype = "dotted", linewidth = 0.8) +
      ggplot2::labs(title = plot_title, x = y_lab, y = "Density") +
      theme_clean
    
    if (!is.null(lower)) p <- p + ggplot2::geom_vline(xintercept = lower, color = "#dc2626", linetype = "dashed", linewidth = 0.8)
    if (!is.null(upper)) p <- p + ggplot2::geom_vline(xintercept = upper, color = "#dc2626", linetype = "dashed", linewidth = 0.8)
    
    return(p)
  }
  
  # Individual Control / Run Chart
  make_control <- function() {
    p <- ggplot2::ggplot(dt, ggplot2::aes(x = obs_id, y = val)) +
      ggplot2::geom_line(color = "#cbd5e1", linewidth = 0.7) +
      ggplot2::geom_point(color = "#334155", size = 2) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = x_bar), color = "#2563eb", linetype = "dotted", linewidth = 0.8) +
      ggplot2::labs(title = plot_title, x = "Index", y = y_lab) +
      theme_clean
    
    if (!is.null(lower)) p <- p + ggplot2::geom_hline(yintercept = lower, color = "#dc2626", linetype = "dashed", linewidth = 0.8)
    if (!is.null(upper)) p <- p + ggplot2::geom_hline(yintercept = upper, color = "#dc2626", linetype = "dashed", linewidth = 0.8)
    
    return(p)
  }
  
  # --- 3. ROUTE PLOT SELECTION ---
  if (plot_type %in% c("hist", "histogram")) {
    return(make_hist())
  } else if (plot_type %in% c("control", "chart", "run")) {
    return(make_control())
  } else if (plot_type %in% c("both")) {
    if (requireNamespace("gridExtra", quietly = TRUE)) {
      return(gridExtra::grid.arrange(make_control(), make_hist(), ncol = 2))
    } else {
      return(make_hist())
    }
  } else {
    return(make_hist())
  }
}