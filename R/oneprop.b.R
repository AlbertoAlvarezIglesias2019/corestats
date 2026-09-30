
# This file is a generated template, your changes will not be overwritten

onepropClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "onepropClass",
    inherit = onepropBase,
    private = list(
      .run = function() {
        
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        
        if (!self$options$input_mode == "use_matrix" && is.null(self$options$variable) ) {
          self$results$tablestyle_ttest$setVisible(FALSE)
          return(FALSE)
        }
        
        if (is.null(self$options$variable) | self$options$input_mode == "use_matrix" ) {
          self$results$tablestyle_boot$setVisible(FALSE)
          self$results$boot_plot_null$setVisible(FALSE)
          self$results$boot_plot_interval$setVisible(FALSE)
        }
        
        
        # 3. Determine column name (falls back to "Response" if variable is NULL in matrix mode)
        var_name <- if (!self$options$input_mode == "use_matrix") {
          self$options$variable
        } else {
          "Response"
        }
        
        
        # Check hypothesised proportion is between 0 and 1
        hp <- as.numeric(self$options$nh_normal)
        hpyn <-  hp>1 | hp<0
        if (hpyn) {
          self$results$tablestyle_ttest$setVisible(FALSE)
          self$results$tablestyle_boot$setVisible(FALSE)
          self$results$warning_message$setContent(
            paste(
              "<p style='color: #D35400; font-size: 1.5em; font-weight: bold;'>",
              "Hypothesised proportion must be a number between 0 and 1",
              "</p>"
            )
          )
          return(FALSE)
        } else {self$results$warning_message$setVisible(FALSE)}
        

        
        # check if x > n
        summ_p_x <- as.numeric(self$options$summ_p_x)
        summ_p_n <- as.numeric(self$options$summ_p_n)        
        if (self$options$input_mode == "use_matrix" && summ_p_x>summ_p_n) {
          self$results$tablestyle_ttest$setVisible(FALSE)
          self$results$warning_message$setContent(
            paste(
              "<p style='color: #D35400; font-size: 1.5em; font-weight: bold;'>",
              "The number of events cannot be greater than the number of trials",
              "</p>"
            )
          )
          self$results$warning_message$setVisible(TRUE)
          return(FALSE)
        } else {self$results$warning_message$setVisible(FALSE)}
        
        
        # 4. Construct data.table dynamically
        if (self$options$input_mode == "use_matrix") {
          dasu <- data.frame(Response = c(rep("Event",summ_p_x),rep("No Event",summ_p_n - summ_p_x)))
          dasu$Response <- factor(dasu$Response,levels = c("Event","No Event"))
          } else {
          dasu <- data.table::as.data.table(self$data[, var_name, drop = FALSE])
          # Ensure grouping column is treated as a factor
          dasu[[var_name]] <- as.factor(dasu[[var_name]])
          dasu[[var_name]] <- relevel(dasu[[var_name]],ref = self$options$refl)
        }
        
        mt <- ifelse(is.null(self$options$miss_text) || self$options$miss_text=="", "Mis",self$options$miss_text )
        ###################################
        ### Creates the output (ttest)
        ###################################
        call_ttest <- rcs_oneprop_normal(data = dasu,
                                        variable = var_name,
                                        conf_normal = as.numeric(self$options$conf_normal)/100,
                                        nh_normal = as.numeric(self$options$nh_normal),
                                        alt_normal = self$options$alt_normal,
                                        nd_num = as.numeric(self$options$nd_num),
                                        font_size = as.numeric(self$options$font_size),
                                        miss_yn = self$options$miss_yn,
                                        meth = self$options$meth,
                                        testyn_normal = self$options$testyn_normal,
                                        miss_text = mt) 
        
        tbl <- as.character(call_ttest$table)
        wrapper_div_style <- "width: 185%; max-width: 1400px; overflow-x: auto;"
        final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
        self$results$tablestyle_ttest$setContent(final_html_output)
        
      
        ###################################
        ### Creates the output (bootstrap)
        ###################################
        if (!self$options$input_mode == "use_matrix" && self$options$boot_yn) {
          call_boot <- rcs_oneprop_boot(data = dasu,
                                        variable = var_name,
                                        conf_boot = as.numeric(self$options$conf_normal)/100,
                                        nh_boot = as.numeric(self$options$nh_normal),
                                        alt_boot = self$options$alt_normal,
                                        nd_num = as.numeric(self$options$nd_num),
                                        font_size = as.numeric(self$options$font_size),
                                        testyn_boot = self$options$testyn_normal,
                                        boot_nullplot_yn = self$options$boot_nullplot_yn,
                                        boot_intervalplot_yn = self$options$boot_intervalplot_yn) 
          
          tbl <- as.character(call_boot$table)
          wrapper_div_style <- "width: 185%; max-width: 1400px; overflow-x: auto;"
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
      })
)
