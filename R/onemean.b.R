
# This file is a generated template, your changes will not be overwritten

onemeanClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "onemeanClass",
    inherit = onemeanBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

          if (!self$options$input_mode == "use_matrix" && is.null(self$options$variable) ) return(FALSE)
          
          if (is.null(self$options$variable) | self$options$input_mode == "use_matrix" ) {
            self$results$tablestyle_tolerance$setVisible(FALSE)
            self$results$tolerance_plot$setVisible(FALSE)
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
          
          # 4. Construct data.table dynamically
          if (self$options$input_mode == "use_matrix") {
            summ_n    <- as.numeric(self$options$summ_n)
            summ_mean <- as.numeric(self$options$summ_mean)
            summ_sd   <- as.numeric(self$options$summ_sd)
            
            # Generate numeric vector scaled to target mean and SD
            sim_values <- as.numeric(scale(stats::rnorm(summ_n))) * summ_sd + summ_mean
            
            dasu <- data.table::data.table(sim_values)
            data.table::setnames(dasu, "sim_values", var_name)
          } else {
            dasu <- data.table::as.data.table(self$data[, var_name, drop = FALSE])
          }
          
          ###################################
          ### Creates the output (ttest)
          ###################################
          call_ttest <- rcs_onemean_ttest(data = dasu,
                                          variable = var_name,
                                          conf_ttest = as.numeric(self$options$conf_ttest)/100,
                                          nh_ttest = as.numeric(self$options$nh_ttest),
                                          alt_ttest = self$options$alt_ttest,
                                          nd_num = as.numeric(self$options$nd_num),
                                          font_size = as.numeric(self$options$font_size),
                                          miss_yn = self$options$miss_yn,
                                          testyn_ttest = self$options$testyn_ttest) 
          
          tbl <- as.character(call_ttest$table)
          wrapper_div_style <- "width: 185%; max-width: 1400px; overflow-x: auto;"
          final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
          self$results$tablestyle_ttest$setContent(final_html_output)

            
          
          ###################################
          ### Creates the output (Tolerance)
          ###################################
          if (!self$options$input_mode == "use_matrix" && self$options$tolerance_yn) {
            
            call_toler <- rcs_onemean_tolerance(data = dasu,
                                                variable = var_name,
                                                conf_tolerance = as.numeric(self$options$conf_tolerance)/100,
                                                poco_tolerance = as.numeric(self$options$poco_tolerance)/100,
                                                side_tolerance = self$options$side_tolerance,
                                                plotype_tolerance = self$options$plotype_tolerance ,
                                                nd_num = as.numeric(self$options$nd_num),
                                                font_size = as.numeric(self$options$font_size)) 

            tbl <- as.character(call_toler$table)
            wrapper_div_style <- "width: 185%; max-width: 1400px; overflow-x: auto;"
            final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
            self$results$tablestyle_tolerance$setContent(final_html_output)
            
            #********************
            #*** Tolerance Plot
            #********************
            if (self$options$tolerance_controlcharts_yn) {
              
              # Let's assume this is where you create your ggplot object
              #final_plot <- call_toler$plot
              tolerance_plot <- call_toler$plot
              #plot(3)
              #recorded_plot <- grDevices::recordPlot()
              
              # --- THIS IS THE CRUCIAL STEP ---
              # 1. Get the image object from the results using its name from the YAML file.
              image <- self$results$tolerance_plot
              
              # 2. Save your created ggplot object to the image's "state".
              #    This makes it available to the renderFun.
              image$setState(tolerance_plot)
            }
          }
          
          
          
          ###################################
          ### Creates the output (bootstrap)
          ###################################
          if (!self$options$input_mode == "use_matrix" && self$options$boot_yn) {
            call_boot <- rcs_onemean_boot(data = dasu,
                                          variable = var_name,
                                          conf_boot = as.numeric(self$options$conf_boot)/100,
                                          nh_boot = as.numeric(self$options$nh_boot),
                                          alt_boot = self$options$alt_boot,
                                          nd_num = as.numeric(self$options$nd_num),
                                          font_size = as.numeric(self$options$font_size),
                                          testyn_boot = self$options$testyn_boot) 

            tbl <- as.character(call_boot$table)
            wrapper_div_style <- "width: 185%; max-width: 1400px; overflow-x: auto;"
            final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
            
            self$results$tablestyle_boot$setContent(final_html_output)
            
            #*************************************
            #*** Bootstrap null distribution plot
            #*************************************
            if (self$options$boot_nullplot_yn) {
              boot_plot <- call_boot$plot_null
              self$results$boot_plot_null$setState(boot_plot)
            }
            #*********************************
            #*** Bootstrap Visualise Interval
            #*********************************
            if (self$options$boot_intervalplot_yn) {
              boot_plot <- call_boot$plot_interval
              self$results$boot_plot_interval$setState(boot_plot)
            }
          }
          
          
          #*******************************
          #*** Normality plot (if chosen)
          #*******************************
          if (!self$options$input_mode == "use_matrix" && self$options$norma_assess_yn) {
              self$results$nomality_plot$setState(dasu[[var_name]])
            }
          

        },
        .boot_plot_null = function(image,...){
          if (is.null(self$options$variable)) return(FALSE)
          if (is.null(image$state))
            return(FALSE)
          
          bp <- image$state
          print(bp)
          return(TRUE)
        },
        .boot_plot_interval = function(image,...){
          if (is.null(self$options$variable)) return(FALSE)
          if (is.null(image$state))
            return(FALSE)
          
          bp <- image$state
          print(bp)
          return(TRUE)
        },
        .tolerance_plot = function(image,...){
          if (is.null(self$options$variable)) return(FALSE)
          if (is.null(image$state))
            return(FALSE)
          
          pt <- rcs_onemean_tolerance_plot(image$state)
          print(pt)
          return(TRUE)
        },
        .nomality_plot = function(image,...){
          if (is.null(self$options$variable) | !self$options$norma_assess_yn) return(FALSE)
          
          x <- image$state
          
          #+++++++++++++++++++++
          #+++ Prepare the data
          #+++++++++++++++++++++
          #var_data <- plotData[[self$options$variable]]
          
          assess_normality(x,self$options$variable)
          TRUE
        })
)
