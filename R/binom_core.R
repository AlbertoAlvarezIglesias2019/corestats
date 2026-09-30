binom_core <- function(x, n, conf.level = 0.95, methods = c("exact", "ac", "asymptotic", "wilson"), ...) {
  
  # Define allowed methods locally to avoid dependency on binom::binom.methods
  binom_methods <- c("agresti-coull", "ac", "asymptotic", "exact", "wilson")
  
  if (length(x) != length(n)) {
    m <- cbind(x = x, n = n)
    x <- m[, "x"]
    n <- m[, "n"]
  }
  
  method <- pmatch(methods, binom_methods)
  if (all(is.na(method))) {
    methods_str <- paste(paste("\"", methods, "\"", sep = ""), collapse = ", ")
    stop("method(s) ", methods_str, "\" not matched")
  }
  if (any(is.na(method))) {
    warning("method(s) ", methods[is.na(method)], " not matched")
    method <- methods[!is.na(method)]
  }
  method <- binom_methods[method]
  
  out <- x > n | x < 0
  if (any(out)) {
    warning("observations with more successes than trials detected and removed")
    x <- x[!out]
    n <- n[!out]
  }
  
  xn <- data.frame(x = x, n = n)
  all.methods <- any(method == "all")
  p <- x / n
  alpha <- 1 - conf.level
  alpha <- rep(alpha, length = length(p))
  alpha2 <- 0.5 * alpha
  z <- qnorm(1 - alpha2)
  z2 <- z * z
  
  res <- NULL
  
  # 1. Agresti-Coull (ac)
  if (any(method %in% c("agresti-coull", "ac")) || all.methods) {
    .x <- x + 0.5 * z2
    .n <- n + z2
    .p <- .x / .n
    lcl <- .p - z * sqrt(.p * (1 - .p) / .n)
    ucl <- .p + z * sqrt(.p * (1 - .p) / .n)
    res.ac <- data.frame(
      method = rep("agresti-coull", NROW(x)), 
      xn, mean = p, lower = lcl, upper = ucl
    )
    res <- res.ac
  }
  
  # 2. Asymptotic (Wald)
  if (any(method == "asymptotic") || all.methods) {
    se <- sqrt(p * (1 - p) / n)
    lcl <- p - z * se
    ucl <- p + z * se
    res.asymp <- data.frame(
      method = rep("asymptotic", NROW(x)), 
      xn, mean = p, lower = lcl, upper = ucl
    )
    res <- if (is.null(res)) res.asymp else rbind(res, res.asymp)
  }
  
  # 3. Exact (Clopper-Pearson)
  if (any(method == "exact") || all.methods) {
    x1 <- x == 0
    x2 <- x == n
    lb <- ub <- x
    lb[x1] <- 1
    ub[x2] <- n[x2] - 1
    lcl <- 1 - qbeta(1 - alpha2, n + 1 - x, lb)
    ucl <- 1 - qbeta(alpha2, n - ub, x + 1)
    if (any(x1)) lcl[x1] <- rep(0, sum(x1))
    if (any(x2)) ucl[x2] <- rep(1, sum(x2))
    res.exact <- data.frame(
      method = rep("exact", NROW(x)), 
      xn, mean = p, lower = lcl, upper = ucl
    )
    res <- if (is.null(res)) res.exact else rbind(res, res.exact)
  }
  
  # 4. Wilson Score
  if (any(method == "wilson") || all.methods) {
    p1 <- p + 0.5 * z2 / n
    p2 <- z * sqrt((p * (1 - p) + 0.25 * z2 / n) / n)
    p3 <- 1 + z2 / n
    lcl <- (p1 - p2) / p3
    ucl <- (p1 + p2) / p3
    res.wilson <- data.frame(
      method = rep("wilson", NROW(x)), 
      xn, mean = p, lower = lcl, upper = ucl
    )
    res <- if (is.null(res)) res.wilson else rbind(res, res.wilson)
  }
  
  attr(res, "conf.level") <- conf.level
  row.names(res) <- seq(nrow(res))
  res
}