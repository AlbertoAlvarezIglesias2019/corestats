rcs_piechart <- function(data,
                         y_axis,
                         panel_by = NULL,
                         miss_yn = FALSE,
                         miss_text = "NA",
                         outcome_text = NULL,
                         font_size = "20",
                         point_size = "4",
                         main_title = NULL,
                         main_subtitle = NULL,
                         panelby_text = NULL,
                         legend_pos = "right",
                         summ_type = "n",
                         nd_num = "2") {

  # Input validation
  if (is.null(y_axis) ) {
    message("Cannot create plot: 'y_axis' must be specified.")
    return(NULL)
  }

  # CRASH-PROOF BASE DATA PREPARATION LAYER
  keep_names <- unlist(list(y_axis = y_axis, panel_by = panel_by))
  indat <- as.data.frame(data)[, keep_names, drop = FALSE]

  # Standardize column naming safely without breaking data.table NSE mechanics
  if (!is.null(panel_by)) {
    colnames(indat) <- c("y_axis_VAR", "panel_by_VAR")
  } else {
    colnames(indat) <- "y_axis_VAR"
  }

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
  nd_num <- honest_numeric(nd_num, default = 0)

  # Set labels and themes
  main_title <- honest_text(main_title,"")
  main_subtitle <- honest_text(main_subtitle,"")
  panelbyt <- honest_text(panelby_text,panel_by)
  outcome_text <- honest_text(outcome_text,y_axis)
  miss_text <- honest_text(miss_text,"(Unknown)")

  # Deal with the missing
  if (!miss_yn) {
    indat <- na.omit(indat)
  } else {
    if (is.factor(indat$y_axis_VAR)) {
      levels(indat$y_axis_VAR) <- unique(c(levels(indat$y_axis_VAR), miss_text))
    }
    indat$y_axis_VAR[is.na(indat$y_axis_VAR)] <- miss_text

    if (!is.null(panel_by)) {
      if (is.factor(indat$panel_by_VAR)) {
        levels(indat$panel_by_VAR) <- unique(c(levels(indat$panel_by_VAR), miss_text))
      }
      indat$panel_by_VAR[is.na(indat$panel_by_VAR)] <- miss_text
    }
  }

  # --- SAFE AGGREGATION & GRAPH ENGINE (IMMUNE TO NSE BUGS) ---
  if (is.null(panel_by)) {
    total_N <- sum(!is.na(indat$y_axis_VAR))

    df <- as.data.frame(table(y_axis_VAR = indat$y_axis_VAR, useNA = "no"))
    colnames(df) <- c("y_axis_VAR", "value")
    df$value <- as.numeric(df$value)
    df$N <- total_N

    if (summ_type == "n") {
      df$label <- ""
    } else if (summ_type == "c") {
      df$label <- paste0(df$value)
    } else if (summ_type == "p") {
      df$label <- paste0(ndformat(df$value / total_N * 100, nd_num), "%")
    } else if (summ_type == "pc") {
      df$label <- paste0(ndformat(df$value / total_N * 100, nd_num), "% (", df$value, ")")
    }

    df <- df[order(df$y_axis_VAR, decreasing = TRUE), ]
    df$ypos <- cumsum(df$value) - 0.5 * df$value

    fp <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = value, fill = y_axis_VAR)) +
      ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::theme_void() +
      ggplot2::geom_text(ggplot2::aes(y = ypos, label = label), color = "white", size = point_size) +
      ggplot2::scale_fill_brewer(palette = "Set1")

  } else {
    df <- as.data.frame(table(y_axis_VAR = indat$y_axis_VAR, panel_by_VAR = indat$panel_by_VAR, useNA = "no"))
    colnames(df) <- c("y_axis_VAR", "panel_by_VAR", "value")
    df$value <- as.numeric(df$value)
    df <- df[df$value > 0, ]

    panel_totals <- aggregate(value ~ panel_by_VAR, data = df, sum)
    df$N <- panel_totals$value[match(df$panel_by_VAR, panel_totals$panel_by_VAR)]

    if (summ_type == "n") {
      df$label <- ""
    } else if (summ_type == "c") {
      df$label <- paste0(df$value)
    } else if (summ_type == "p") {
      df$label <- paste0(ndformat(df$value / df$N * 100, nd_num), "%")
    } else if (summ_type == "pc") {
      df$label <- paste0(ndformat(df$value / df$N * 100, nd_num), "% (", df$value, ")")
    }

    df <- df[order(df$panel_by_VAR, df$y_axis_VAR, decreasing = c(FALSE, TRUE)), ]
    df$ypos <- unsplit(lapply(split(df, df$panel_by_VAR), function(sub) {
      cumsum(sub$value) - 0.5 * sub$value
    }), df$panel_by_VAR)

    fp <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = value, fill = y_axis_VAR)) +
      ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::theme_void() +
      ggplot2::geom_text(ggplot2::aes(y = ypos, label = label), color = "white", size = point_size) +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      ggplot2::facet_wrap(~panel_by_VAR, scales = "free", ncol = 2)
  }

  ylabe <- outcome_text
  fp <- fp + ggplot2::labs(y = ylabe)

  if (main_title != "") {
    fp <- fp + ggplot2::ggtitle(main_title, subtitle = main_subtitle) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), plot.subtitle = ggplot2::element_text(hjust = 0.5))
  }

  #fp <- fp + ggplot2::labs(fill = outcome_text)
  if (!is.null(panel_by)) {
    fp <- fp + ggplot2::labs(fill = outcome_text, caption = paste("Paneled by:", panelbyt))
  } else {
    fp <- fp + ggplot2::labs(fill = outcome_text)
  }
  
  fp <- fp + ggplot2::theme(text = ggplot2::element_text(size = font_size), legend.position = legend_pos)

  return(fp)
}
