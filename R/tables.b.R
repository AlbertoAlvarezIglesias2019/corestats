
# This file is a generated template, your changes will not be overwritten

tablesClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "tablesClass",
    inherit = tablesBase,
    private = list(
        .run = function() {

          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          if (is.null(self$options$row) || is.null(self$options$col) ) {
            return()
          }
          
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          newtable <- rcs_tables(self$data,
                                 row = self$options$row,
                                 col = self$options$col,
                                 percent = self$options$perc,
                                 marg  = self$options$marg,
                                 miss_yn = self$options$miss_yn,
                                 miss_text = self$options$miss_text,
                                 margin_text = self$options$margin_text,
                                 cols_text = self$options$cols_text,
                                 rows_text = self$options$rows_text,
                                 addp = self$options$addp,
                                 font_size = self$options$font_size,
                                 main_title = self$options$main_title,
                                 nd_cat = self$options$nd_cat)
          
          
          
          newtable <- as.character(newtable$table)
          #newtable <- as.character(newtable)
          wrapper_div_style <- "width: 185%; max-width: 1400px; overflow-x: auto;"
          final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, newtable)
          
          self$results$tablestyle$setContent(final_html_output1)

        })
)
