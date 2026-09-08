piechartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "piechartClass",
  inherit = piechartBase,
  private = list(
    .run = function() {

      plot_data <- NULL

      # ===================================================================
      # 1. DATA ROUTING & VALIDATION LAYER
      # ===================================================================
      if (self$options$input_mode == "use_variables") {

        # Guard: If no variable is selected, hide plot and exit safely
        if (is.null(self$options$y_axis)) {
          self$results$plot$setVisible(FALSE)
          return()
        }
        plot_data <- self$data

      } else {
        # Mode: use_matrix (Summarized Data Table)
        if (is.null(self$options$table_json) || self$options$table_json == "") {
          self$results$plot$setVisible(FALSE)
          return()
        }

        # Parse the user-submitted JSON string matrix
        matrix_list <- jsonlite::fromJSON(self$options$table_json)
        mat <- matrix_list$matrix
        dimnames(mat) <- list(
          Response = matrix_list$row_labels,
          Groups   = matrix_list$col_labels
        )

        # Flatten 2D contingency table into counts data
        df_counts <- as.data.frame.table(mat, responseName = "Freq")
        df_counts$Response <- as.character(df_counts$Response)
        df_counts$Groups   <- as.character(df_counts$Groups)

        # Expand counts into a clean case-level data frame
        plot_data <- df_counts[rep(seq_len(nrow(df_counts)), df_counts$Freq), c("Groups", "Response")]
        rownames(plot_data) <- NULL

        plot_data$Groups <- factor(plot_data$Groups,levels = matrix_list$col_labels)
        plot_data$Response <- factor(plot_data$Response,levels = matrix_list$row_labels)

        # Guard: If table contains all zeros, data frame has 0 rows. Hide plot and exit.
        if (is.null(plot_data) || nrow(plot_data) == 0) {
          self$results$plot$setVisible(FALSE)
          return()
        }
      }

      # ===================================================================
      # 2. STATE COMMITTAL & VISIBILITY GATE
      # ===================================================================
      # We passed all guards! Safe to activate container and pass the data frame
      self$results$plot$setVisible(TRUE)

      image <- self$results$plot
      image$setState(plot_data)

    },

    .plot = function(image, ...){
      # Direct exit if there is no data state ready to render
      if (is.null(image$state) || nrow(as.data.frame(image$state)) == 0) return(FALSE)

      # Determine column targets based on active mode
      if (self$options$input_mode == "use_matrix") {
        y_axis_target   <- "Response"
        panel_by_target <- if (length(unique(image$state$Groups)) > 1) "Groups" else NULL
      } else {
        y_axis_target   <- self$options$y_axis
        panel_by_target <- self$options$panel_by
      }

      # Build core argument list payload
      args <- list(
        data          = image$state,
        y_axis        = y_axis_target,
        panel_by      = panel_by_target,
        miss_yn       = self$options$miss_yn,
        miss_text     = self$options$miss_text,
        outcome_text   = self$options$outcome_text,
        font_size     = self$options$font_size,
        point_size    = self$options$point_size,
        main_title    = self$options$main_title,
        main_subtitle = self$options$main_subtitle,
        panelby_text  = self$options$panelby_text,
        legend_pos    = self$options$legend_pos,
        summ_type     = self$options$summ_type,
        nd_num       = self$options$nd_num
      )

      # Generate the ggplot visualization
      fp <- do.call(corestats::rcs_piechart, args)

      print(fp)
      return(TRUE)
    })
)
