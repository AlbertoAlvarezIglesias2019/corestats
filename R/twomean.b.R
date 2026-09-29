
# This file is a generated template, your changes will not be overwritten

twomeanClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "twomeanClass",
    inherit = twomeanBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          
          # 1. Early Return Guard: Check if required inputs are missing
          if (self$options$input_mode != "use_matrix") {
            if (is.null(self$options$variable) || is.null(self$options$by)) {
              self$results$warning_message$setVisible(FALSE)
              self$results$tablestyle_ttest_1$setVisible(FALSE)
              self$results$tablestyle_ttest_2$setVisible(FALSE)
              self$results$tablestyle_boot$setVisible(FALSE)
              self$results$boot_plot_null$setVisible(FALSE)
              self$results$boot_plot_interval$setVisible(FALSE)
              self$results$nomality_plot$setVisible(FALSE)
              return(FALSE)
            }
          } else {
            # In matrix mode, hide variable-dependent plots/tables
            self$results$tablestyle_boot$setVisible(FALSE)
            self$results$boot_plot_null$setVisible(FALSE)
            self$results$boot_plot_interval$setVisible(FALSE)
            self$results$nomality_plot$setVisible(FALSE)
          }
          
          
          # 3. Determine column name (falls back to "Response" if variable is NULL in matrix mode)
          var_name <- if (!self$options$input_mode == "use_matrix") {
            self$options$variable
          } else {
            "Response"
          }
          grp_name <- if (!self$options$input_mode == "use_matrix") {
            self$options$by
          } else {
            "Group"
          }
          
          
          # 4. Construct data.table dynamically
          if (self$options$input_mode == "use_matrix") {
            summ1_n    <- as.numeric(self$options$summ1_n)
            summ1_mean <- as.numeric(self$options$summ1_mean)
            summ1_sd   <- as.numeric(self$options$summ1_sd)
            summ2_n    <- as.numeric(self$options$summ2_n)
            summ2_mean <- as.numeric(self$options$summ2_mean)
            summ2_sd   <- as.numeric(self$options$summ2_sd)
            
            # Generate data
            dasu1 <- data.frame(Response = scale(rnorm(summ1_n))*summ1_sd+summ1_mean,
                                Group = "Group 1")
            dasu2 <- data.frame(Response = scale(rnorm(summ2_n))*summ2_sd+summ2_mean,
                                Group = "Group 2")
            dasu <- rbind(dasu1,dasu2)
            dasu$Group <- factor(dasu$Group)
            
          } else {
            # Safely extract dataset columns as character vector
            raw_data <- self$data[, c(var_name, grp_name), drop = FALSE]
            dasu <- data.table::as.data.table(raw_data)
            
            # Ensure grouping column is treated as a factor
            dasu[[grp_name]] <- as.factor(dasu[[grp_name]])
            dasu[[grp_name]] <- relevel(dasu[[grp_name]],ref = self$options$refl)
          }
          
          
          num_levels <- nlevels(dasu[[grp_name]])
          
          if (num_levels != 2) {
            self$results$tablestyle_ttest_1$setVisible(FALSE)
            self$results$tablestyle_ttest_2$setVisible(FALSE)
            self$results$nomality_plot$setVisible(FALSE)
            self$results$tablestyle_boot$setVisible(FALSE)
            self$results$boot_plot_null$setVisible(FALSE)
            self$results$boot_plot_interval$setVisible(FALSE)
            
            self$results$warning_message$setContent(
              paste(
                "<p style='color: #D35400; font-size: 1.5em; font-weight: bold;'>",
                "The selected Grouping variable must have exactly two levels.\n",
                "Found:", num_levels, "level(s).",
                "</p>"
              )
            )
            
            return(FALSE)
          } else {self$results$warning_message$setVisible(FALSE)}
          
          
          
          ###################################
          ### Creates the output (ttest)
          ###################################
          call_ttest <- rcs_twomean_ttest(data = dasu,
                                          variable = var_name,
                                          by = grp_name,
                                          conf_ttest = as.numeric(self$options$conf_ttest)/100,
                                          nh_ttest = as.numeric(self$options$nh_ttest),
                                          alt_ttest = self$options$alt_ttest,
                                          ev = self$options$ev,
                                          nd_num = as.numeric(self$options$nd_num),
                                          font_size = as.numeric(self$options$font_size),
                                          miss_yn = self$options$miss_yn,
                                          testyn_ttest = self$options$testyn_ttest,
                                          miss_text = self$options$miss_text) 
          
          tbl1 <- as.character(call_ttest$table_summ)
          tbl2 <- as.character(call_ttest$table_infe)
          
          wrapper_div_style <- "width: 185%; max-width: 1400px; overflow-x: auto;"
          final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl1)
          final_html_output2 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl2)
          
          self$results$tablestyle_ttest_1$setContent(final_html_output1)
          self$results$tablestyle_ttest_2$setContent(final_html_output2)
            
          
          ###################################
          ### Creates the output (bootstrap)
          ###################################
          if (!self$options$input_mode == "use_matrix" && self$options$boot_yn) {
            call_boot <- rcs_twomean_boot(data = dasu,
                                          variable = var_name,
                                          by = grp_name,
                                          conf_boot = as.numeric(self$options$conf_boot)/100,
                                          nh_boot = as.numeric(self$options$nh_boot),
                                          alt_boot = self$options$alt_boot,
                                          nd_num = as.numeric(self$options$nd_num),
                                          font_size = as.numeric(self$options$font_size),
                                          testyn_boot = self$options$testyn_boot,
                                          boot_nullplot_yn = self$options$boot_nullplot_yn,
                                          boot_intervalplot_yn = self$options$boot_intervalplot_yn) 
            
            tbl <- as.character(call_boot$table)
            final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
            
            self$results$tablestyle_boot$setContent(final_html_output)
            
            #*************************************
            #*** Bootstrap null distribution plot
            #*************************************
            #if (self$options$boot_nullplot_yn) {
            #  boot_plot <- call_boot$plot_null
              self$results$boot_plot_null$setState(call_boot$plot_null)
            #}
            #*********************************
            #*** Bootstrap Visualise Interval
            #*********************************
            #if (self$options$boot_intervalplot_yn) {
            #  boot_plot <- call_boot$plot_interval
              self$results$boot_plot_interval$setState(call_boot$plot_interval)
            #}
          }
          

          #*******************************
          #*** Normality plot (if chosen)
          #*******************************          
          if (self$options$norma_assess_yn && self$options$input_mode != "use_matrix") {
            # Pass dataset AND variable names together in image$state
            plot_state <- list(
              data = dasu,
              var_name = var_name,
              grp_name = grp_name
            )
            self$results$nomality_plot$setState(plot_state)
          }
          
          
          },
        .boot_plot_null = function(image,...){
          if (is.null(self$options$variable)) return(FALSE)
          if (is.null(image$state)) return(FALSE)
          
          bp <- image$state
          print(bp)
          return(TRUE)
        },
        .boot_plot_interval = function(image,...){
          if (is.null(self$options$variable)) return(FALSE)
          if (is.null(image$state)) return(FALSE)
          
          bp <- image$state
          print(bp)
          return(TRUE)
        },
        .nomality_plot = function(image,...){
            if (is.null(self$options$variable) | !self$options$norma_assess_yn) return(FALSE)
            if (is.null(image$state)) return(FALSE)
            
            # Retrieve variable names and dataset from image$state
            DD <- image$state$data
            var_name <- image$state$var_name
            grp_name <- image$state$grp_name
            
            # Extract factor levels
            l1 <- levels(DD[[grp_name]])[1]
            l2 <- levels(DD[[grp_name]])[2]
            
            # Extract column vectors safely and convert explicitly to numeric
            var_vec <- unlist(DD[[var_name]])
            grp_vec <- unlist(DD[[grp_name]])
            
            x1 <- as.numeric(var_vec[grp_vec == l1])
            x2 <- as.numeric(var_vec[grp_vec == l2])
            
            
            #+++++++++++++++++++++
            #+++ Prepare the data
            #+++++++++++++++++++++
            #x1, x2, var_name1 = "Data 1", var_name2 = "Data 2"
            assess_normality_2var(x1,x2,l1,l2)
            TRUE
          }) 
          

)
