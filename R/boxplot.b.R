
# This file is a generated template, your changes will not be overwritten

boxplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "boxplotClass",
    inherit = boxplotBase,
    private = list(
      .run = function() {

        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        if (is.null(self$options$y_axis)) {
          self$results$plot$setVisible(FALSE)
          return()
        }


        image <- self$results$plot
        image$setState(self$data)

      },
      .plot = function(image, ...){
        if (is.null(self$options$y_axis)) return(FALSE)

        args <- list(
          y_axis        = self$options$y_axis,
          x_axis        = self$options$x_axis,
          color_by      = self$options$color_by,
          panel_by      = self$options$panel_by,
          miss_yn       = self$options$miss_yn,
          miss_text     = self$options$miss_text,
          x_axis_text   = self$options$x_axis_text,
          y_axis_text   = self$options$y_axis_text,
          y_axis_logyn  = self$options$y_axis_logyn,
          font_size     = self$options$font_size,
          point_size    = self$options$point_size,
          line_size     = self$options$line_size,
          flip_yn       = self$options$flip_yn,
          addData_yn    = self$options$addData_yn,
          line_yn       = self$options$line_yn,
          line_val      = self$options$line_val,
          y_inc         = self$options$y_inc,
          main_title    = self$options$main_title,
          main_subtitle = self$options$main_subtitle,
          legend_title  = self$options$legend_title,
          panelby_text  = self$options$panelby_text,
          legend_pos    = self$options$legend_pos
        )


        args$data <- image$state

        fp <- do.call(corestats::rcs_boxplot,args)

        print(fp)
        return(TRUE)

      })
)
