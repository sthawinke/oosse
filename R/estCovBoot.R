#' Estimate covariance between proportion and binomial probability
#'
#'Estimate covariance between pihat and Pr(pihat>0.5) through the bootstrap
#'
#' @return The estimated covariance
#' @inheritParams oosse
#' @importFrom stats pbinom cov
estCovBoot = function(y, nBootstraps){
    n = length(y)
    tmp = vapply(integer(nBootstraps), FUN.VALUE = double(2), function(j){
        ySam = sample(y, replace = TRUE)
        prob = pbinom(n/2, size = n, prob = piHat <- mean(ySam))
        c("piHat" = piHat, "prob" = prob)
    })
    cov(tmp[1,], tmp[2,])
}