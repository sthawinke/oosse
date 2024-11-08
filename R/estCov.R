#' Estimate covariance between proportion and binomial probability or log probability
#'
#'Estimate covariance between pihat and Pr(pihat>0.5) analytically
#'
#' @param n the sample size
#' @param p the success probability
#' @param what Which correlation should be calculated. See details
#' @return The estimated covariance
#' @importFrom stats pbinom cov dbinom
#' @details The correlation can be calculated between \eqn{\hat{p}} and \eqn{\widehat{Pr}(\hat{p}>0.5)} ("probability"),
#' or between \eqn{\hat{p}} and \eqn{\log(\hat{p})} or \eqn{1-\hat{p}} and \eqn{\log(1-\hat{p})} ("logLoss" and "logLossMin" respectively).
estCov = function(n, p, what = c("probability", "logLoss", "logLossMin")){
    what = match.arg(what)
    seqK = switch(what, "probability" = 0:n, seq_len(n-1))
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

#' Estimate covariance between the average prediction (\eqn{\hat{\kappa}}) and
#' the average outcome (\eqn{\bar{y}}) through the bootstrap.
#'
#' @inheritParams oosse
#'
#' @return The estimated covariance
estCovKappaY = function(y, x, fitFun, predFun, nBootstraps){
    n = NROW(y)
    booIns = vapply(integer(nBootstraps), FUN.VALUE = double(2), function(bb){
        id = sample(n, replace = TRUE)
        c(yBar = mean(y[id]), kappaHat = mean(predFun(fitFun(y[id], x[id,,drop = FALSE]), x[id,,drop = FALSE])))
    })
    cov(booIns[1, ], booIns[2,], use = "pairwise.complete.obs")
}