#' Estimate covariance between proportion and binomial probability
#'
#'Estimate covariance between pihat and Pr(pihat>0.5) analytically or through the bootstrap
#'
#' @return The estimated covariance
#' @inheritParams oosse
#' @importFrom stats pbinom cov dbinom
estCov = function(n, p, what = c("probability", "logLoss", "logLossMin")){
    what = match.arg(what)
    seqK = switch(what, "probability" = 0:n,  1:(n-1))
    xx <- seqK/n
    yy <- switch(what,
                 "probability" = pbinom(n/2, size = n, prob = xx, lower.tail = TRUE),
                 "logLoss" = log(xx), "logLossMin" = log(1-xx)
    )
    dp = dbinom(seqK, size = n, prob = p)
    if(what == "logLossMin"){
        xx = 1-xx
    }
    sum(dp*(xx -sum(xx*dp))*(yy - sum(yy*dp)))*n/(n-1)
}