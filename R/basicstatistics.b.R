
# This file is a generated template, your changes will not be overwritten

basicstatisticsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "basicstatisticsClass",
    inherit = basicstatisticsBase,
    private = list(
      .run = function() {
        
        if (is.null(self$options$outcome)) {
          return()
        }
        

        #+++++++++++++++++++++
        #+++ Prepare the data
        #+++++++++++++++++++++
        varsName <- self$options$outcome
        groupName <- self$options$group_by
        #tmpDat <- self$data
        tmpDat <- jmvcore::select(self$data, c(varsName,groupName))
        tmpDat <- as.data.frame(tmpDat)
        
  
        suco <- self$options$suco
        if (self$options$allco) suco <- "all"
        
        
        #- name: mist
        #title: Missing Label
        #type: String
        miss_text <- "(Unknown)"
        if (!is.null(self$options$miss_text)) miss_text <- self$options$miss_text
        
        
        #- name: varl
        #title: "Label: Variable/s"
        #type: String
        #default: "Characteristic"
        outcome_text <- self$options$outcome_text
        if (is.null(outcome_text)) outcome_text <- "Characteristic"

        
        ###########
        ### Check 
        ###########
        newtable <- rcs_basicstatistics(tmpDat,outcome = self$options$outcome,
                                        group_by=self$options$group_by,
                                        miss_yn=self$options$miss_yn,
                                        miss_text = miss_text,
                                        suca = self$options$suca,
                                        suco = suco,
                                        addp = self$options$addp,
                                        addt = self$options$addt,
                                        addn = self$options$addn,
                                        outcome_text = outcome_text,
                                        groupby_text = self$options$groupby_text,
                                        font_size = self$options$font_size,
                                        main_title = self$options$main_title,
                                        nd_num = self$options$nd_num,
                                        nd_cat = self$options$nd_cat)

        
        newtable <- as.character(newtable)
        wrapper_div_style <- "width: 185%; max-width: 1400px; overflow-x: auto;"
        final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, newtable)
        
        self$results$tablestyle$setContent(final_html_output1)
        
        
      })
)
