#' Estimate covariance between proportion and binomial probability
#'
#'Estimate covariance between pihat and Pr(pihat>0.5) analytically or through the bootstrap
#'
#' @return The estimated covariance
#' @inheritParams oosse
#' @importFrom stats pbinom cov dbinom
estCov = function(y, estCovMethod = "analytical", nBootstraps){
    n = length(y)
    if(estCovMethod == "bootstrap"){
        tmp = vapply(integer(nBootstraps), FUN.VALUE = double(2), function(j){
            ySam = sample(y, replace = TRUE)
            prob = pbinom(n/2, size = n, prob = piHat <- mean(ySam))
            c("piHat" = piHat, "prob" = prob)
        })
        cov(tmp[1,], tmp[2,])
    } else if(estCovMethod == "analytical"){
        p = mean(y)
        seqK = 0:n;xx <- seqK/n
        yy <- pbinom(n/2, size = n, prob = xx, lower.tail = TRUE)
        dp = dbinom(seqK, size = n, prob = p)
        sum(dp*(xx -sum(xx*dp))*(yy - sum(yy*dp)))*n/(n-1)
    }
}