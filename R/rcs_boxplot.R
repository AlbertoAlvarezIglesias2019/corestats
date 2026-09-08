#' @title Generate a Box Plot with Multiple Customization Options
#'
#' @description This function generates a highly customizable box plot based on the
#'    logic from a Jamovi module. It handles various layout options such as grouping,
#'    color mapping, faceting, log transformations, horizontal reference lines,
#'    and optional raw data point overlays.
#'
#' @param data A data frame or data.table containing the variables to plot.
#' @param y_axis A character string specifying the name of the continuous variable to be plotted on the y-axis. (Required)
#' @param x_axis An optional character string specifying the categorical variable for the x-axis. If NULL, a single box plot is drawn. Default is `NULL`.
#' @param color_by An optional character string specifying the name of the variable to use for outline/point color. Default is `NULL`.
#' @param panel_by An optional character string specifying the name of the variable to use for faceting the plot. Default is `NULL`.
#' @param miss_yn A logical value. If TRUE, missing values are labeled with `miss_text`; otherwise, they are omitted via `na.omit()`. Default is `FALSE`.
#' @param miss_text A character string specifying the label for missing values. Default is `"NA"`.
#' @param x_axis_text An optional character string for the custom x-axis label. If NULL, `x_axis` is used. Default is `NULL`.
#' @param y_axis_text An optional character string for the custom y-axis label. If NULL, `y_axis` is used. Default is `NULL`.
#' @param y_axis_logyn A logical value. If TRUE, the y-axis is log-transformed. Default is `FALSE`.
#' @param font_size A character string or numeric value controlling the plot text font size. Default is `"20"`.
#' @param point_size A character string or numeric value controlling the point size for jittered data. Default is `"4"`.
#' @param line_size A character string or numeric value controlling the boxplot outline thickness. Default is `"2"`.
#' @param flip_yn A logical value. If TRUE, the plot axes are flipped horizontally. Default is `FALSE`.
#' @param addData_yn A logical value. If TRUE, overlays jittered raw data points onto the box plot layout. Default is `FALSE`.
#' @param line_yn A logical value. If TRUE, adds a horizontal dashed reference line to the plot. Default is `FALSE`.
#' @param line_val A character string or numeric value specifying the y-intercept position of the reference line. Default is `FALSE` (defaults internally to 0).
#' @param y_inc An optional character string or numeric value for custom Y-axis step increments. Default is `NULL`.
#' @param main_title An optional character string for the main plot title. Default is `NULL`.
#' @param main_subtitle An optional character string for the plot subtitle. Default is `NULL`.
#' @param legend_title An optional character string for the legend title. Default is `NULL`.
#' @param panelby_text An optional character string providing a custom label prefix for the facet panels. Default is `NULL`.
#' @param legend_pos A character string for the legend position. Accepts standard ggplot2 positions (e.g., `"right"`, `"left"`, `"top"`, `"bottom"`, `"none"`). Default is `"right"`.
#'
#' @return A `ggplot` object, which can be printed, modified, or saved.
#'
#' @details This function is a standalone version of a Jamovi module and
#'    reproduces its box plot functionality. It uses `ggplot2` for all plotting
#'    and `data.table` for fast internal data wrangling.
#'
#' @examples
#' # Use the built-in mtcars dataset
#' data(mtcars)
#'
#' # Ensure grouping variables are factors for clean box plots
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$am  <- as.factor(mtcars$am)
#'
#' # 1. A basic single box plot of miles per gallon (mpg)
#' rcs_boxplot(
#'    data = mtcars,
#'    y_axis = "mpg"
#' )
#'
#' # 2. Box plot separated by a categorical X-axis variable (cylinders)
#' mtcars$cyl <- as.character(mtcars$cyl)
#' rcs_boxplot(
#'    data = mtcars,
#'    x_axis = "cyl",
#'    y_axis = "mpg"
#' )
#'
#' # 3. Advanced box plot with overlaid raw jitter data points and custom titles
#' mtcars$am <- as.character(mtcars$am)
#' rcs_boxplot(
#'    data = mtcars,
#'    x_axis = "cyl",
#'    y_axis = "mpg",
#'    color_by = "am",
#'    addData_yn = TRUE,
#'    main_title = "MPG Distribution by Engine Attributes",
#'    main_subtitle = "Grouped by transmission type",
#'    legend_title = "Transmission"
#' )
#'
#' # 4. Box plot featuring a horizontal reference line and custom Y step increments
#' rcs_boxplot(
#'    data = mtcars,
#'    x_axis = "cyl",
#'    y_axis = "wt",
#'    line_yn = TRUE,
#'    line_val = 3.5,
#'    y_inc = 0.5,
#'    y_axis_text = "Weight (1000 lbs)"
#' )
#'
rcs_boxplot <- function(data,
                        y_axis,
                        x_axis = NULL,
                        color_by = NULL,
                        panel_by = NULL,
                        miss_yn = FALSE,
                        miss_text = "NA",
                        x_axis_text = NULL,
                        y_axis_text = NULL,
                        y_axis_logyn = FALSE,
                        font_size = "20",
                        point_size = "4",
                        line_size = "2",
                        flip_yn = FALSE,
                        addData_yn = FALSE,
                        line_yn = FALSE,
                        line_val = FALSE,
                        y_inc=NULL,
                        main_title = NULL,
                        main_subtitle = NULL,
                        legend_title = NULL,
                        panelby_text = NULL,
                        legend_pos = "right") {


  # Input validation
  if (is.null(y_axis) ) {
    message("Cannot create plot: 'y_axis' must be specified.")
    return(NULL)
  }

  library(data.table)

  # Prepare data
  #indat <- data.table::setDT(data)
  #cols <- c(x_axis = x_axis, y_axis = y_axis, color_by = color_by, panel_by = panel_by)
  #indat <- indat[, cols, with = FALSE]
  #data.table::setnames(indat, names(cols))

  # CRASH-PROOF DATA PREPARATION LAYER
  # 1. Gather all non-NULL variables requested by the user
  keep_names <- unlist(list(x_axis = x_axis, y_axis = y_axis, color_by = color_by, panel_by = panel_by))
  # 2. Extract using explicit base R formatting to ensure safety inside jamovi's plot environment
  indat <- data.table::as.data.table(as.data.frame(data)[, keep_names, drop = FALSE])
  # 3. Rename columns seamlessly to their standardized internal reference variables
  data.table::setnames(indat, old = keep_names, new = names(keep_names))


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
  line_size  <- honest_numeric(line_size, default = 1.7)
  line_val  <- honest_numeric(line_val, default = 0)

  # Parse out step increments safely (defaults to NULL so it skips unless user inputs values)
  y_inc_num  <- honest_numeric(y_inc, default = NULL)



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
    if (!is.null(x_axis)) {
      if (is.factor(indat$x_axis)) {
        levels(indat$x_axis) <- unique(c(levels(indat$x_axis), miss_text))
      }
      indat$x_axis[is.na(indat$x_axis)] <- miss_text
      #indat[is.na(color_by), color_by := miss_text]
    }

    # 1. Handle color_by (Skip if NULL or numeric)
    if (!is.null(color_by)) {
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
    }
  }




  # Build the base ggplot object
  if (!is.null(x_axis)) {
    fp <- ggplot2::ggplot(indat, ggplot2::aes(x = x_axis, y = y_axis))
  } else {
    fp <- ggplot2::ggplot(indat, ggplot2::aes(x = 1, y = y_axis)) + ggplot2::scale_x_discrete() +ggplot2::xlab(NULL)
  }

  fp <- fp + ggplot2::geom_boxplot(size = line_size)


  # Dynamically append color mapping if specified
  if (!is.null(color_by)) {
    fp <- fp + ggplot2::aes(colour = color_by)
  }

  # --- CONSOLIDATED X-AXIS SCALE CONFIGURATION ---
  xlabe <- x_axis_text

  # --- CONSOLIDATED Y-AXIS SCALE CONFIGURATION ---
  ylabe <- y_axis_text
  y_trans <- "identity"
  if (y_axis_logyn) {
    y_trans <- "log"
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

  # Reference line
  if (line_yn) {
    fp <- fp +
      ggplot2::geom_hline(yintercept = line_val, linetype = "dashed", color = "black", linewidth = line_size) +
      ggplot2::annotate("label", x = 0, y = line_val, label = "Reference line",
                        hjust = 1.1, vjust = 1.2, size = 5, color = "black",
                        fill = "white", alpha = 0.8)
  }

  # Add data
  if (addData_yn) {
    if (!is.null(color_by)) {
      fp <- fp + ggplot2::geom_jitter(position=ggplot2::position_dodge2(0.8,padding=0.05),shape = 5,size=point_size)
    } else {
      fp <- fp + ggplot2::geom_jitter(width = 0.2,shape = 5,size = point_size)
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

