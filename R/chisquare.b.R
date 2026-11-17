
# This file is a generated template, your changes will not be overwritten

chisquareClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "chisquareClass",
    inherit = chisquareBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          
          # 3. Determine column name (falls back to "Response" if variable is NULL in matrix mode)
          if (self$options$input_mode == "use_matrix") {
            rrr_name <- "Rows"
            ccc_name <- "Cols"
          } else {
            rrr_name <- self$options$rrr
            ccc_name <- self$options$ccc
          }
          
          
          # ===================================================================
          # 1. DATA ROUTING & VALIDATION LAYER
          # ===================================================================
          if (self$options$input_mode == "use_variables") {
            
            # Guard: If no variable is selected, hide plot and exit safely
            if (is.null(self$options$rrr) || is.null(self$options$ccc)) {
              self$results$data_plot$setVisible(FALSE)
              return()
            }
            
            
            raw_data <- self$data[, c(rrr_name, ccc_name), drop = FALSE]
            dasu <- data.table::as.data.table(raw_data)
            
            # Ensure grouping column is treated as a factor
            dasu[[ccc_name]] <- as.factor(dasu[[ccc_name]])
            dasu[[ccc_name]] <- relevel(dasu[[ccc_name]],ref = self$options$refc)
            
            dasu[[rrr_name]] <- as.factor(dasu[[rrr_name]])
            dasu[[rrr_name]] <- relevel(dasu[[rrr_name]],ref = self$options$refr)
            
          } else {
            # Mode: use_matrix (Summarized Data Table)
            if (is.null(self$options$table_json) || self$options$table_json == "") {
              self$results$data_plot$setVisible(FALSE)
              return()
            }
            
            # Parse the user-submitted JSON string matrix
            matrix_list <- jsonlite::fromJSON(self$options$table_json)
            mat <- matrix_list$matrix
            dimnames(mat) <- list(
              Rows = matrix_list$row_labels,
              Cols   = matrix_list$col_labels
            )
            
            # Flatten 2D contingency table into counts data
            df_counts <- as.data.frame.table(mat, responseName = "Freq")
            df_counts$Rows <- as.character(df_counts$Rows)
            df_counts$Cols   <- as.character(df_counts$Cols)
            
            # Expand counts into a clean case-level data frame
            dasu <- df_counts[rep(seq_len(nrow(df_counts)), df_counts$Freq), c("Cols", "Rows")]
            rownames(dasu) <- NULL
            
            dasu$Cols <- factor(dasu$Cols,levels = matrix_list$col_labels)
            dasu$Rows <- factor(dasu$Rows,levels = matrix_list$row_labels)

            # Guard: If table contains all zeros, data frame has 0 rows. Hide plot and exit.
            if (is.null(dasu) || nrow(dasu) == 0) {
              self$results$data_plot$setVisible(FALSE)
              return()
            }
          }
          
          
          # Helper function to inject equal column widths into any HTML table
          make_equal_cols <- function(html_table, num_cols, col_width_px = 110) {
            # Build <col> elements with fixed pixel width
            cols <- paste0(rep(sprintf('<col style="width: %dpx;">', col_width_px), num_cols), collapse = "")
            colgroup <- paste0('<colgroup>', cols, '</colgroup>')
            
            # Set table-layout: fixed with auto width so table grows/shrinks naturally
            html_table <- sub('(<table[^>]*)(style="[^"]*")?', '\\1 style="table-layout: fixed; width: auto; max-width: 100%;"', html_table)
            html_table <- sub('(<table[^>]*>)', paste0('\\1', colgroup), html_table)
            
            return(html_table)
          }
        
          ###################################
          ### Creates the output (chi square)
          ###################################
          cols_text <- ifelse(is.null(self$options$cols_text) || self$options$cols_text=="", ccc_name,self$options$cols_text )
          rows_text <- ifelse(is.null(self$options$rows_text) || self$options$rows_text=="", rrr_name,self$options$rows_text )
          miss_text <- ifelse(is.null(self$options$miss_text) || self$options$miss_text=="", "Unknown",self$options$miss_text )
          
          args_list <- list(
            data = dasu,
            rrr = rrr_name,
            ccc = ccc_name,
            nd_num = as.numeric(self$options$nd_num),
            font_size = as.numeric(self$options$font_size),
            miss_yn = self$options$miss_yn,
            addfisher_yn = self$options$addfisher_yn,
            rows_text = rows_text,
            cols_text = cols_text,
            miss_text = miss_text
          )
          
          call_test <- do.call(rcs_chisquare_test, args_list)

          tbl1 <- as.character(call_test$table_summ)
          tbl2 <- as.character(call_test$table_infe)
          
          tbl1 <- make_equal_cols(tbl1, num_cols = call_test$nc-1)
          
          #css_style <- "<style> table td, table th { vertical-align: top !important; } </style>"
          wrapper_div_style <- "width: 185%; max-width: 1400px; overflow-x: auto;"
          final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl1)
          final_html_output2 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl2)
          
          self$results$tablestyle_test_1$setContent(final_html_output1)
          self$results$tablestyle_test_2$setContent(final_html_output2)
          
          #*******************************
          #*** Data plot (if chosen)
          #*******************************
          if (self$options$plotdata_yn) {
            #dat_plot <- call_test$plot_data
            #self$results$data_plot$setState(call_test$plot_data)  
            self$results$data_plot$setState(list(data = dasu,variable = rrr_name, by = ccc_name,rrr_lab=rows_text,ccc_lab=cols_text)) 
            
          } 
          

        },
        .ghostplot = function(image,...){
          return(FALSE)
        },
        .data_plot = function(image,...){
          #if (is.null(self$options$rrr) ) return(FALSE)
          if (is.null(image$state)) return(FALSE)
          
          pt <- rcs_chisquare_data_plot(data  = image$state$data,
                                       rrr = image$state$variable,  #self$options$rrr,
                                       ccc = image$state$by ,
                                       rrr_lab = image$state$rrr_lab,
                                       ccc_lab = image$state$ccc_lab)  #self$options$ccc)
          print(pt)
          return(TRUE)
        })
)
