#' @title Generate a Scatter Plot with Multiple Customization Options
#'
#' @description This function generates a customizable scatter plot based on the
#'    logic from a Jamovi module. It handles various plot options such as color
#'    mapping, faceting, transformations, best-fit lines, and plot aesthetics.
#'
#' @param data A data frame or data.table containing the variables to plot.
#' @param x_axis A character string specifying the name of the variable to be plotted on the x-axis.
#' @param y_axis A character string specifying the name of the variable to be plotted on the y-axis.
#' @param color_by An optional character string specifying the name of the variable to use for point color. Default is `NULL`.
#' @param panel_by An optional character string specifying the name of the variable to use for faceting the plot. Default is `NULL`.
#' @param miss_yn A logical value. If TRUE, missing values are labeled with `miss_text`; otherwise, they are omitted via `na.omit()`. Default is `FALSE`.
#' @param miss_text A character string specifying the label for missing values. Default is `"NA"`.
#' @param x_axis_text An optional character string for the custom x-axis label. If NULL, `x_axis` is used. Default is `NULL`.
#' @param y_axis_text An optional character string for the custom y-axis label. If NULL, `y_axis` is used. Default is `NULL`.
#' @param x_axis_logyn A logical value. If TRUE, the x-axis is log-transformed. Default is `FALSE`.
#' @param y_axis_logyn A logical value. If TRUE, the y-axis is log-transformed. Default is `FALSE`.
#' @param font_size A character string or numeric value controlling the plot text font size. Default is `"20"`.
#' @param point_size A character string or numeric value controlling the point size. Default is `"4"`.
#' @param line_size A character string or numeric value controlling the line thickness. Default is `"2"`.
#' @param flip_yn A logical value. If TRUE, the plot axes are flipped. Default is `FALSE`.
#' @param same_scale_yn A logical value. If TRUE, the x and y axes are set to the same scale range. Default is `FALSE`.
#' @param line_eq_yn A logical value. If TRUE, a dashed line of equality (y=x) is added to the plot. Default is `FALSE`.
#' @param x_inc An optional character string or numeric value for custom X-axis step increments. Default is `NULL`.
#' @param y_inc An optional character string or numeric value for custom Y-axis step increments. Default is `NULL`.
#' @param best_fit_type A character string for the best-fit line. Must be `"none"`, `"reg"` (linear model), or `"smo"` (smooth loess). Default is `"none"`.
#' @param smooth_span A character string or numeric value specifying the smoothing alpha/span for loess. Must be >= 0.1 to be plotted. Default is `"0.75"`.
#' @param best_fit_label A logical value. If FALSE, the centered annotation labels for smoothers and regression lines disappear. Default is `TRUE`.
#' @param best_fit_int_yn A logical value. If TRUE, adds a confidence interval ribbon to the best-fit line. Default is `FALSE`.
#' @param main_title An optional character string for the main plot title. Default is `NULL`.
#' @param main_subtitle An optional character string for the plot subtitle. Default is `NULL`.
#' @param legend_title An optional character string for the legend title. Default is `NULL`.
#' @param panelby_text An optional character string providing a custom label prefix for the facet panels. Default is `NULL`.
#' @param legend_pos A character string for the legend position. Accepts standard ggplot2 positions (e.g., `"right"`, `"left"`, `"top"`, `"bottom"`, `"none"`). Default is `"right"`.
#'
#' @return A `ggplot` object, which can be printed, modified, or saved.
#'
#' @details This function is a standalone version of a Jamovi module and
#'    reproduces its plot functionality. It uses `ggplot2` for all plotting
#'    and `data.table` for fast internal data wrangling.
#'
#' @examples
#' # Use the built-in mtcars dataset
#' data(mtcars)
#'
#' # 1. A basic scatter plot of mpg vs. wt
#' rcs_scatterplot(
#'    data = mtcars,
#'    x_axis = "wt",
#'    y_axis = "mpg"
#' )
#'
#' # 2. A more complex plot with color, linear regression, and custom labels
#' rcs_scatterplot(
#'    data = mtcars,
#'    x_axis = "wt",
#'    y_axis = "mpg",
#'    color_by = "cyl",
#'    best_fit_type = "reg",
#'    best_fit_int_yn = TRUE,
#'    main_title = "MPG vs. Weight",
#'    legend_title = "Cylinders"
#' )
#'
#' # 3. A plot with a log-transformed y-axis and a smooth best-fit line
#' rcs_scatterplot(
#'    data = mtcars,
#'    x_axis = "hp",
#'    y_axis = "mpg",
#'    y_axis_logyn = TRUE,
#'    best_fit_type = "smo",
#'    font_size = "24"
#' )
#'
#' # 4. A regression line plot with the overlay text label hidden
#' rcs_scatterplot(
#'    data = mtcars,
#'    x_axis = "hp",
#'    y_axis = "mpg",
#'    x_axis_logyn = TRUE,
#'    best_fit_type = "reg",
#'    best_fit_label = FALSE,
#'    font_size = "24"
#' )
#'
#' # 5. Advanced formatting utilizing custom line scaling parameters and increments
#' rcs_scatterplot(
#'    data = mtcars,
#'    x_axis = "wt",
#'    y_axis = "mpg",
#'    x_axis_logyn = FALSE,
#'    best_fit_type = "none",
#'    best_fit_label = FALSE,
#'    font_size = "16",
#'    line_eq_yn = TRUE,
#'    same_scale_yn = TRUE,
#'    point_size = "2",
#'    line_size = "1",
#'    x_inc = 1,
#'    y_inc = 5
#' )
#'
rcs_scatterplot <- function(data, x_axis,
                            y_axis,
                            color_by = NULL,
                            panel_by = NULL,
                            miss_yn = FALSE,
                            miss_text = "NA",
                            x_axis_text = NULL,
                            y_axis_text = NULL,
                            x_axis_logyn = FALSE,
                            y_axis_logyn = FALSE,
                            font_size = "20",
                            point_size = "4",
                            line_size = "2",
                            flip_yn = FALSE,
                            same_scale_yn = FALSE,
                            line_eq_yn = FALSE,
                            x_inc=NULL,
                            y_inc=NULL,
                            best_fit_type = "none",
                            smooth_span="0.75",
                            best_fit_label=TRUE,
                            best_fit_int_yn = FALSE,
                            main_title = NULL,
                            main_subtitle = NULL,
                            legend_title = NULL,
                            panelby_text = NULL,
                            legend_pos = "right") {


  # Input validation
  if (is.null(x_axis) || is.null(y_axis)) {
    message("Cannot create plot: 'x_axis' and 'y_axis' must be specified.")
    return(NULL)
  }

  library(data.table)

  # Prepare data
  indat <- data.table::as.data.table(data)
  cols <- c(x_axis = x_axis, y_axis = y_axis, color_by = color_by, panel_by = panel_by)
  indat <- indat[, cols, with = FALSE]
  data.table::setnames(indat, names(cols))
  isnum_color_by <- is.numeric(indat$color_by)

  # Helper inline function to handle safe conversion
  honest_numeric <- function(val, default) {
    if (is.null(val)) return(default)
    num <- suppressWarnings(as.numeric(val))
    if (is.na(num) || num <= 0) return(default)
    return(num)
  }

  honest_text <- function(val, default) {
    if (is.null(val)) return(default)
    tex <- as.character(val)
    if (is.na(tex) || tex == "") return(default)
    return(tex)
  }

  # Sizes conversion & safety fallbacks
  font_size  <- honest_numeric(font_size, default = 14)
  point_size <- honest_numeric(point_size, default = 2.5)
  line_size  <- honest_numeric(line_size, default = 1)
  span_num   <- honest_numeric(smooth_span, default = 0.75)

  # Parse out step increments safely (defaults to NULL so it skips unless user inputs values)
  x_inc_num  <- honest_numeric(x_inc, default = NULL)
  y_inc_num  <- honest_numeric(y_inc, default = NULL)


  # Safeguard boundary validation for loess span
  if (span_num <= 0.1) span_num <- 0.2

  # Set labels and themes
  main_title <- honest_text(main_title,"")
  main_subtitle <- honest_text(main_subtitle,"")
  leg_title <- honest_text(legend_title,color_by)
  panelbyt <- honest_text(panelby_text,panel_by)
  x_axis_text <- honest_text(x_axis_text,x_axis)
  y_axis_text <- honest_text(y_axis_text,y_axis)
  miss_text <- honest_text(miss_text,"(Unknown)")

  # Deal with the missing
  if (!miss_yn) {indat <- na.omit(indat)
  } else {
    # 1. Handle color_by (Skip if NULL or numeric)
    if (!is.null(color_by) && !isnum_color_by) {
      if (is.factor(indat$color_by)) {
        levels(indat$color_by) <- unique(c(levels(indat$color_by), miss_text))
      }
      indat$color_by[is.na(indat$color_by)] <- miss_text
      #indat[is.na(color_by), color_by := miss_text]
    }

    # 2. Handle panel_by (Skip only if NULL)
    if (!is.null(panel_by)) {
      if (is.factor(indat$panel_by)) {
        levels(indat$panel_by) <- unique(c(levels(indat$panel_by), miss_text))
      }
      indat$panel_by[is.na(indat$panel_by)] <- miss_text
      #indat[is.na(panel_by), panel_by := miss_text]
    }
  }




  # Build the base ggplot object
  fp <- ggplot2::ggplot(indat, ggplot2::aes(x = x_axis, y = y_axis)) +
    ggplot2::geom_point(size = point_size)

  # Dynamically append color mapping if specified
  if (!is.null(color_by)) {
    fp <- fp + ggplot2::aes(colour = color_by)
  }

  # --- CONSOLIDATED X-AXIS SCALE CONFIGURATION ---
  xlabe <- x_axis_text
  x_trans <- "identity"
  if (x_axis_logyn) {
    x_trans <- "log"
    #xlabe <- paste0("log (", xlabe, ")")
  }

  x_breaks <- ggplot2::waiver()
  if (!is.null(x_inc_num)) {
    x_breaks <- function(limits) {
      if (any(!is.finite(limits))) return(ggplot2::waiver())
      scales::breaks_width(x_inc_num)(limits)
    }
  }
  # Single, clean call using 'trans' to cover older ggplot versions smoothly
  fp <- fp + ggplot2::scale_x_continuous(trans = x_trans, breaks = x_breaks)

  # --- CONSOLIDATED Y-AXIS SCALE CONFIGURATION ---
  ylabe <- y_axis_text
  y_trans <- "identity"
  if (y_axis_logyn) {
    y_trans <- "log"
    #ylabe <- paste0("log (", ylabe, ")")
  }

  y_breaks <- ggplot2::waiver()
  if (!is.null(y_inc_num)) {
    y_breaks <- function(limits) {
      if (any(!is.finite(limits))) return(ggplot2::waiver())
      scales::breaks_width(y_inc_num)(limits)
    }
  }
  # Single, clean call using 'trans' to cover older ggplot versions smoothly
  fp <- fp + ggplot2::scale_y_continuous(trans = y_trans, breaks = y_breaks)


  # Apply compiled labels
  fp <- fp + ggplot2::labs(y = ylabe,x = xlabe)


  # Add other plot elements
  if (flip_yn) fp <- fp + ggplot2::coord_flip()

  # Same scale
  if (same_scale_yn) {
    limits <- range(indat$x_axis, indat$y_axis, na.rm = TRUE)
    fp <- fp + ggplot2::expand_limits(x = limits, y = limits)
  }

  # Line of equility
  if (line_eq_yn) {
    fp <- fp +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = line_size) +
      ggplot2::annotate("label", x = Inf, y = Inf, label = "Line of equality",
                        hjust = 1.1, vjust = 1.2, size = 5, color = "black",
                        fill = "white", alpha = 0.8)
  }


  # Add fit line trends
  if (best_fit_type %in% c("reg", "smo")) {

      # 1. Dynamically set configuration options based on type
      fit_method <- if (best_fit_type == "reg") "lm" else "loess"
      fit_color  <- if (best_fit_type == "reg") "blue" else "red"
      label_txt  <- if (best_fit_type == "reg") "Regression line" else "Smoother"

      # 2. Render plot layers based on grouping rules
      if (is.null(color_by) || isnum_color_by) {
        # Add a single overall styled line
        fp <- fp + ggplot2::geom_smooth(method = fit_method,
                                        se = best_fit_int_yn,
                                        span=span_num,
                                        linewidth = line_size, color = fit_color)

        # Optional centered label
        if (best_fit_label) {
          fp <- fp + ggplot2::annotate(
            "label",
            x = mean(indat$x_axis, na.rm = TRUE),
            y = mean(indat$y_axis, na.rm = TRUE),
            label = label_txt,
            fontface = "bold",
            size = 5,
            color = "white",
            fill = fit_color
          )
        }

      } else {
        # Add grouped lines (colors are mapped automatically by ggplot)
        fp <- fp + ggplot2::geom_smooth(method = fit_method, se = best_fit_int_yn,span=span_num, linewidth = line_size)
      }

      }

  # Render titles and themes
  if (main_title != "") {
    fp <- fp + ggplot2::ggtitle(main_title, subtitle = main_subtitle) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), plot.subtitle = ggplot2::element_text(hjust = 0.5))
  }

  #leg_title <- if (!is.null(color_by)) color_by else legend_title
  fp <- fp + ggplot2::labs(color = leg_title)
  fp <- fp + ggplot2::theme(text = ggplot2::element_text(size = font_size), legend.position = legend_pos)

  # Add facets
  if (!is.null(panel_by)) {
    fp <- fp + ggplot2::facet_wrap(~panel_by, labeller = function(labels) {
      names(labels) <- panelbyt  # Overwrites the prefix with your custom text
      ggplot2::label_both(labels)
    })
  }

  return(fp)
}

