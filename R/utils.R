html_code_function <- function(tbl,tasi,tasi_width) {
  html_code <- tbl %>%
    gt::tab_options(
      table.width = tasi_width,
      #table.width = pct(100),
      #container.width = pct(100),
      table.layout = "auto",
      data_row.padding = gt::px(4),
      #footnotes.multiline = FALSE,
      table.font.size = tasi)%>%
    gt::as_raw_html()
  
  html_code <- paste0(
    '<div style="width: 120%; max-width: 1000px; overflow-x: auto;">',
    html_code,
    '</div>'
  )
  html_code
}


tasi_width_function <- function(tbl,tamano) {
  nc <- ncol(tbl$table_body) - 6
  #exfa <- round(1 + (1/(8-2)*nc-2/(8-2)),2)

  ts <- round((120-30)/9*nc + 30,0)
  tm <- round((160-40)/9*nc + 40,0)
  tl <- round((200-50)/9*nc + 50,0)
  
  tamanonum <- case_when(tamano=="ts"~ts,
                         tamano=="tm"~tm,
                         tamano=="tl"~tl)
  #tamanonum <- tamanonum*exfa
  paste(round(tamanonum,0),"%",sep="")
}


mis_label_function <- function(x,mist) {
  
  if (class(x) == "factor" | class(x) == "ordered" ) {lll <- levels(x)} else {lll <- unique(x)}
  lll <- lll[!is.na(lll)]
  lll <- c(lll,mist)
  x <- as.character(x)
  x <- dplyr::if_else(is.na(x),mist,x)
  x <- factor(x,levels = lll)
  x
}


pvformat <- function(pv) {
  temp <- sapply(pv,function(ppvv) {
    if (is.na(ppvv)) return("")
    number <- round(ppvv, 3)
    pvalue <- dplyr::if_else(ppvv < 0.001, "<0.001", sprintf("%.3f", number))
    pvalue <- dplyr::if_else(ppvv > 0.999, ">0.999", pvalue)
    pvalue
  })
  as.character(temp)
}

#ndformat <- function(x,nd) {
#  temp <- sapply(x,function(xx){
#    if (is.na(xx)) return(" ")
#    number <- round(xx, nd)
#    sprintf(paste("%.",nd,"f",sep=""), number)
#  })
#  as.character(temp)
#
#}


ndformat <- function(x, nd) {
  # Pre-allocate character vector filled with spaces for NA values
  res <- character(length(x))
  na_idx <- is.na(x)
  res[na_idx] <- " "
  
  # Format non-NA values vectorized
  if (any(!na_idx)) {
    fmt <- paste0("%.", nd, "f")
    res[!na_idx] <- sprintf(fmt, round(x[!na_idx], nd))
  }
  
  return(res)
}






#' @title Calculate Bonferroni-Adjusted Confidence Intervals for Pairwise Proportions
#'
#' @description This function performs pairwise comparisons of proportions for two
#' categorical variables. It uses `rstatix::pairwise_prop_test()` to get the pairs
#' and the total number of comparisons, then manually runs `prop.test()` for each
#' pair, applying a Bonferroni correction to the confidence level to obtain adjusted
#' confidence intervals.
#'
#' @param data A data frame containing the two categorical variables.
#' @param ccc A character string specifying the name of the first categorical variable (column variable).
#' @param rrr A character string specifying the name of the second categorical variable (row variable).
#'
#' @return A `data.frame` with the following columns:
#'   \itemize{
#'     \item `group1`: The first group in the comparison.
#'     \item `group2`: The second group in the comparison.
#'     \item `estimate`: The difference in proportions between the two groups.
#'     \item `conf.low`: The lower bound of the Bonferroni-adjusted confidence interval.
#'     \item `conf.high`: The upper bound of the Bonferroni-adjusted confidence interval.
#'   }
#'
#' @details
#' The function first identifies all pairwise combinations of the levels of the
#' `ccc` variable. For each pair, it calculates the counts for the first level
#' of the `rrr` variable. It then applies a Bonferroni correction to the
#' confidence level (alpha = 0.05) by dividing it by the total number of
#' comparisons. This adjusted confidence level is used in `stats::prop.test()`
#' to calculate the confidence interval for the difference in proportions, resulting
#' in a wider, more conservative interval that accounts for multiple testing.
#'
#' @seealso \code{\link[rstatix]{pairwise_prop_test}}, \code{\link[stats]{prop.test}}
#'
#' @examples
#' # Create a sample data frame
#' data_df <- data.frame(
#'   gender = factor(c(rep("Male", 30), rep("Female", 20),"Binary")),
#'   opinion = factor(c(rep("Agree", 15), rep("Disagree", 15), rep("Agree", 10), rep("Disagree", 10),"Disagree"))
#' )
#'
#' # Calculate the adjusted confidence intervals
#' intervals <- getintervals_bc(
#'   data = data_df,
#'   ccc = "gender",
#'   rrr = "opinion"
#' )
#'
#' # Print the results
#' print(intervals)
getintervals_bc <- function(data, ccc, rrr) {
  
  
  df <- data %>%
    dplyr::select(all_of(c(rrr,ccc) )) %>% 
    na.omit()
  
  colu <- droplevels(df[[ccc]])
  rows <- droplevels(df[[rrr]])
  rows1 <- dplyr::if_else(rows == levels(rows)[1],levels(rows)[1],"OOtthheerr")
  rows1 <- factor(rows1,levels = c(levels(rows)[1],"OOtthheerr"))
  
  xtab <- table(rows1,colu)
  
  #colu <- data[[ccc]]
  #rows <- data[[rrr]]
  #rows1 <- dplyr::if_else(rows == levels(rows)[1],levels(rows)[1],"Other")
  #rows1 <- factor(rows1,levels = c(levels(rows)[1],"Other"))
  #xtab <- table(rows1,colu)
  
  fit <- t(xtab) %>% rstatix::pairwise_prop_test()
  
  
  ######################
  ### Get the intervals
  ######################
  temp <- lapply(1:dim(fit)[1],function(iii){
    g1 <- fit$group1[iii]
    g2 <- fit$group2[iii]
    df1 <- data %>% 
      dplyr::filter(.data[[ccc]] %in% c(g1,g2) ) %>% 
      dplyr::count(.data[[ccc]])
    df2 <- data %>% 
      dplyr::filter(.data[[ccc]] %in% c(g1,g2) ) %>% 
      dplyr::count(.data[[ccc]],.data[[rrr]])
    
    x1 <- df2 %>% dplyr::filter(.data[[ccc]]==g1 & .data[[rrr]]==levels(rows)[1]) %>% pull(n)
    if (length(x1)==0) x1 <- 0
    n1 <- df1 %>% dplyr::filter(.data[[ccc]]==g1) %>% pull(n)
    x2 <- df2 %>% dplyr::filter(.data[[ccc]]==g2 & .data[[rrr]]==levels(rows)[1]) %>% pull(n)
    if (length(x2)==0) x2 <- 0
    n2 <- df1 %>% dplyr::filter(.data[[ccc]]==g2) %>% pull(n)
    
    ## Correct for bonferroni
    n_comparisons <- nrow(fit)
    alpha <- 0.05
    conf_level_adj <- 1 - (alpha / n_comparisons)
    
    # Run the test
    ptest <- prop.test(c(x1,x2),c(n1,n2),conf.level=conf_level_adj)
    
    # Calculate and format descriptive statistics.
    data.frame(group1=g1,
               group2=g2,
               estimate = ptest$estimate[1] - ptest$estimate[2],
               conf.low = ptest$conf.int[1],
               conf.high = ptest$conf.int[2])
  })
  out <- do.call("rbind",temp)
  row.names(out) <- NULL
  out
}



wald_prop_diff_ci <- function(x1, n1, x2, n2, conf.level = 0.95, alternative = c("two.sided", "less", "greater")) {
  # Input Validation
  alternative <- match.arg(alternative)
  if (conf.level <= 0 | conf.level >= 1) {
    stop("conf.level must be between 0 and 1.")
  }
  if (x1 < 0 | x2 < 0 | n1 <= 0 | n2 <= 0 | x1 > n1 | x2 > n2) {
    stop("Invalid values for successes (x) or trials (n).")
  }
  
  # 1. Calculate the core components
  p1_hat <- x1 / n1
  p2_hat <- x2 / n2
  diff_prop <- p1_hat - p2_hat
  
  # Standard Error (SE) using unpooled sample variances (for the CI)
  SE <- sqrt( (p1_hat * (1 - p1_hat) / n1) + (p2_hat * (1 - p2_hat) / n2) )
  
  # Initialize CI bounds
  lower_bound <- -1
  upper_bound <- 1
  
  # Determine the critical Z-value based on the alternative
  if (alternative == "two.sided") {
    # For a two-sided interval, use Z-score for alpha/2
    alpha <- 1 - conf.level
    z_critical <- qnorm(1 - alpha / 2)
  } else {
    # For one-sided intervals, use Z-score for alpha
    alpha <- 1 - conf.level
    z_critical <- qnorm(1 - alpha)
  }
  
  # Calculate the Margin of Error (ME)
  ME <- z_critical * SE
  
  # 2. Calculate the confidence interval based on the alternative
  if (alternative == "two.sided") {
    lower_bound <- diff_prop - ME
    upper_bound <- diff_prop + ME
  } else if (alternative == "greater") {
    # Lower bound for p1 - p2 > L
    lower_bound <- diff_prop - ME
    upper_bound <- 1  # Theoretical maximum difference is 1
  } else if (alternative == "less") {
    # Upper bound for p1 - p2 < U
    lower_bound <- -1 # Theoretical minimum difference is -1
    upper_bound <- diff_prop + ME
  }
  
  # 3. Format the results
  result <- data.frame(
    Estimate = diff_prop,
    SE = SE,
    Lower_CI = max(-1, lower_bound),  # Constrain to [-1, 1]
    Upper_CI = min(1, upper_bound),    # Constrain to [-1, 1]
    Conf_Level = conf.level,
    Alternative = alternative
  )
  
  # Set the row name
  row.names(result) <- "p1 - p2"
  
  return(result)
}
