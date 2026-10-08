
# This file is a generated template, your changes will not be overwritten

anovaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "anovaClass",
    inherit = anovaBase,
    private = list(
      .run = function() {
        
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        
        varsName <- self$options$variable
        byName <- self$options$by
        
        # Initial check: No variables selected
        if (is.null(varsName) || is.null(byName)) {
          self$results$warning_message_1$setVisible(FALSE)
          self$results$warning_message_2$setVisible(FALSE)
          self$results$tablestyle_test_1$setVisible(FALSE)
          self$results$tablestyle_test_2$setVisible(FALSE)
          self$results$tablestyle_ph$setVisible(FALSE)
          self$results$residual_plot$setVisible(FALSE)
          self$results$data_plot$setVisible(FALSE)
          self$results$ph_interval_plot$setVisible(FALSE)
          self$results$ph_inference_plot$setVisible(FALSE)
          return()
        }
        
        # Prepare the data efficiently using data.table
        dt <- data.table::as.data.table(jmvcore::select(self$data, c(varsName, byName)))
        dt[[byName]] <- factor(dt[[byName]])
        dt[[byName]] <- stats::relevel(dt[[byName]], ref = self$options$refl)
        dt[[byName]] <- mis_label_function(dt[[byName]], "Unknown")
        
        if (!self$options$miss_yn) {
          dt <- dt[get(byName) != "Unknown"]
          dt[[byName]] <- droplevels(dt[[byName]])
        }
        tmpDat <- as.data.frame(dt)
        
        nd <- as.numeric(self$options$nd_num)
        font_size <- as.numeric(self$options$font_size)
        ev <- self$options$ev
        
        by_levels_table <- table(tmpDat[[byName]])
        
        # Check condition for warning message 1 (<= 2 levels)
        if (length(by_levels_table) <= 2) {
          self$results$warning_message_2$setVisible(FALSE)
          self$results$warning_message_1$setContent(
            "<p style='color: #D35400; font-size: 1.5em; font-weight: bold;'>The selected Grouping variable has less than three levels.<br>Only three or more levels are supported by this analysis.</p>"
          )
          self$results$warning_message_1$setVisible(TRUE)
          return()
        }
        
        # Check condition for warning message 2 (less than 2 observations per level)
        if (any(by_levels_table < 2)) {
          self$results$warning_message_1$setVisible(FALSE)
          self$results$warning_message_2$setContent(
            "<p style='color: #4D9900; font-size: 1.5em; font-weight: bold;'>One of the levels in the grouping variable has less than 2 observations.<br>Only three or more observations per level are supported by this analysis.</p>"
          )
          self$results$warning_message_2$setVisible(TRUE)
          return()
        }
        
        
        # Clear warnings if conditions are met
        self$results$warning_message_1$setVisible(FALSE)
        self$results$warning_message_2$setVisible(FALSE)
        
        mt <- ifelse(is.null(self$options$miss_text) || self$options$miss_text=="", "Mis",self$options$miss_text )
        # Run ANOVA test and populate tables
        call_test <- rcs_anova_test(
          data = tmpDat,
          variable = varsName,
          by = byName,
          nd_num = nd,
          ev = ev,
          font_size = font_size,
          miss_yn = self$options$miss_yn,
          miss_text = mt)
        
        wrapper_div_style <- "width: 185%; max-width: 1400px; overflow-x: auto;"
        self$results$tablestyle_test_1$setContent(sprintf('<div style="%s">%s</div>', wrapper_div_style, as.character(call_test$table_summ)))
        self$results$tablestyle_test_2$setContent(sprintf('<div style="%s">%s</div>', wrapper_div_style, as.character(call_test$table_infe)))
        
        # Optional plots state updates
        if (self$options$norma_assess_yn) {
          self$results$residual_plot$setState(self$data)
        }
        
        if (self$options$plotdata_yn) {
          self$results$data_plot$setState(tmpDat)
        }       
        
        
        # Post-Hoc Analysis
        if (self$options$ph_yn) {
          call_ph <- rcs_anova_posthoc(
            data = tmpDat,
            variable = varsName,
            by = byName,
            nd_num = nd,
            ev = ev,
            font_size = font_size,
            miss_yn = self$options$miss_yn,
            type = self$options$method
          )
          self$results$tablestyle_ph$setContent(sprintf('<div style="%s">%s</div>', wrapper_div_style, as.character(call_ph)))
        }
        
        if (self$options$ph_yn && self$options$ph_plot_intervals_yn) {
          self$results$ph_interval_plot$setState(self$data)
        }
        
        if (self$options$ph_yn && self$options$ph_plot_significance_yn) {
          self$results$ph_inference_plot$setState(tmpDat)
        }
        
        
      },
      .ghostplot = function(image,...){
        return(FALSE)
      },
      .residual_plot = function(image,...){
        if (is.null(self$options$variable) || is.null(self$options$by) || is.null(image$state)) return(FALSE)
        pt <- rcs_anova_residual_plot(
          data = image$state,
          variable = self$options$variable,
          by = self$options$by
        )
        print(pt)
        return(TRUE)
      },
      .data_plot = function(image,...){
        if (is.null(self$options$variable) || is.null(image$state)) return(FALSE)
        pt <- rcs_anova_data_plot(
          data = image$state,
          variable = self$options$variable,
          by = self$options$by
          )
        print(pt)
        return(TRUE)
      },
      .ph_interval_plot = function(image,...){
        if (is.null(self$options$variable) || is.null(image$state)) return(FALSE)
        pt <- rcs_anova_ph_interval_plot(
          data = image$state,
          variable = self$options$variable,
          by = self$options$by,
          type = self$options$method
        )
        print(pt)
        return(TRUE)
      },
      .ph_inference_plot = function(image,...){
        if (is.null(self$options$variable) || is.null(image$state)) return(FALSE)
        pt <- rcs_anova_ph_inference_plot(
          data = image$state,
          variable = self$options$variable,
          by = self$options$by,
          type = self$options$method
        )
        print(pt)
        return(TRUE)
      })
)
