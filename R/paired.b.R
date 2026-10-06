
# This file is a generated template, your changes will not be overwritten

pairedClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pairedClass",
    inherit = pairedBase,
    private = list(
      .run = function() {
        
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)

        if (is.null(self$options$var1) | is.null(self$options$var2)) {
          self$results$tablestyle_ttest$setVisible(FALSE)
          self$results$tablestyle_boot$setVisible(FALSE)
          self$results$boot_plot_interval$setVisible(FALSE)
          self$results$boot_plot_null$setVisible(FALSE)
          self$results$data_plot$setVisible(FALSE)
          return(NULL)
        }
        
        
        #+++++++++++++++++++++
        #+++ Prepare the data
        #+++++++++++++++++++++
        #if (!is.null(self$options$var1) & !is.null(self$options$var2)) {
        #  var1nam  <- self$options$var1
        #  var2nam  <- self$options$var2
        #  
        #  tmpDat <- jmvcore::select(self$data, c(var1nam,var2nam) )
        #  tmpDat <- as.data.frame(tmpDat)
        #  var1 <- tmpDat[[var1nam]]
        #  var2 <- tmpDat[[var2nam]]
        #}
        var1 <- self$data[[self$options$var1]]
        var2 <- self$data[[self$options$var2]]
        
      
        mt <- ifelse(is.null(self$options$miss_text) || self$options$miss_text=="", "Mis",self$options$miss_text )
        call_ttest <- rcs_paired_ttest(var1,
                                       var2, 
                                       conf_ttest = as.numeric(self$options$conf_ttest)/100,
                                       nh_ttest = as.numeric(self$options$nh_ttest),
                                       alt_ttest = self$options$alt_ttest,
                                       nd_num = self$options$nd_num,
                                       font_size = self$options$font_size,
                                       miss_yn = self$options$miss_yn,
                                       testyn_ttest = self$options$testyn_ttest,
                                       miss_text = mt) 
          
        
        tbl <- as.character(call_ttest$table)
        wrapper_div_style <- "width: 185%; max-width: 1400px; overflow-x: auto;"
        final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
        self$results$tablestyle_ttest$setContent(final_html_output)
        
        
        
        ###################################
        ### Creates the output (bootstrap)
        ###################################
        if (self$options$boot_yn) {
          
          call_boot <- rcs_paired_boot(var1,
                                       var2,
                                       conf_boot = as.numeric(self$options$conf_ttest)/100,
                                       alt_boot = self$options$alt_ttest,
                                       nd_num = self$options$nd_num,
                                       font_size = self$options$font_size,
                                       testyn_boot = self$options$testyn_ttest,
                                       boot_nullplot_yn = self$options$boot_nullplot_yn,
                                       boot_intervalplot_yn = self$options$boot_intervalplot_yn)
          
          tbl <- as.character(call_boot$table)
          final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
          self$results$tablestyle_boot$setContent(final_html_output)
          
          #*************************************
          #*** Bootstrap null distribution plot
          #*************************************
          #if (self$options$boot_yn && self$options$boot_nullplot_yn) {
            #boot_plot <- call_boot$plot_null
            self$results$boot_plot_null$setState(call_boot$plot_null)
          #}
          
          #*********************************
          #*** Bootstrap Visualise Interval
          #*********************************
          #if (self$options$boot_yn) {
            self$results$boot_plot_interval$setState(call_boot$plot_interval)
          #}
        }
        
        
        #*******************************
        #*** Visualise plot (if chosen)
        #*******************************
        if (!is.null(self$options$var1) & !is.null(self$options$var1)) {
          if (self$options$plotdata_yn) {
            #image <- self$results$nomality_plot
            #image$setState(self$data)
            self$results$data_plot$setState(self$data)
          }
        }
        
      },
      .boot_plot_null = function(image,...){
        if (is.null(self$options$var1) | is.null(self$options$var2)) return(FALSE)
        if (is.null(image$state))
          return(FALSE)
        
        bp <- image$state
        print(bp)
        return(TRUE)
      },
      .boot_plot_interval = function(image,...){
        if (is.null(self$options$var1) | is.null(self$options$var2)) return(FALSE)
        if (is.null(image$state))
          return(FALSE)
        
        bp <- image$state
        print(bp)
        return(TRUE)
      },
      .data_plot = function(image,...){
        if (is.null(self$options$var1) | is.null(self$options$var2) | !self$options$plotdata_yn) return(FALSE)
        
        plotData <- image$state
        
        #+++++++++++++++++++++
        #+++ Prepare the data
        #+++++++++++++++++++++
        var1 <- plotData[[self$options$var1]]
        var2 <- plotData[[self$options$var2]]
        
        #pd <- PairedData::paired(var1, var2)
        #names(pd) <- c(self$options$var1,self$options$var2)
        #out <- PairedData::plot(pd,type = "profile") + theme_bw()
        #print(out)
        
        par(mar = c(5, 6, 4, 2), cex.lab = 1.4, cex.axis = 1.3, font.lab = 2, bg = "white")
        
        # 3. Create the base plot layout
        plot(NA, xlim = c(0.8, 2.2), ylim = range(c(var1, var2), na.rm = TRUE),
             xaxt = "n", xlab = "", ylab = "Value", bty = "l", main = "")
        
        # Add subtle horizontal grid lines
        grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted", lwd = 1)
        
        # 4. Draw ALL profile lines at once using vectorized segments() instead of a for loop
        segments(x0 = 1, y0 = var1, x1 = 2, y1 = var2, 
                 col = adjustcolor("gray50", alpha.f = 0.5), lwd = 2)
        
        # 5. Add data points (points() is already vectorized)
        points(rep(1, length(var1)), var1, col = "#2b5c8f", pch = 19, cex = 1.4)
        points(rep(2, length(var1)), var2, col = "#d95f02", pch = 19, cex = 1.4)
        
        # 6. Customize X-axis labels
        axis(1, at = c(1, 2), labels = c(self$options$var1, self$options$var2), tick = FALSE, line = 0.5)
        
        TRUE
      })
)
