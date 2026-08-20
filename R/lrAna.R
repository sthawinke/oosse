#' Analytically estimate out-of-sample binary reference loss and its standard error
#' for the Heidke and Peirce skill scores.
#'
#' @param yBar The average outcome
#' @param kappaHat The average prediction
#' @param covEst The estimated covariance between estimators for yBar and kappaHat
#'
#' @return The reference loss
#' @inheritParams getSEsNested
lrAnaBinMod <- function(yBar, kappaHat, n, covEst) {
  yBar * (1 - kappaHat) + (1 - yBar) * kappaHat + 2 * covEst
  #deltaSE <- NA # Mathematically intractable due to weird definition (Stephenson)
  #c(lrAna, deltaSE)
  # ADD: standard error of the full HSS, see Hogan 2009
}
lrAnaBinObs <- function(yBar, n) {
  lrAna <- 2 * yBar * (1 - yBar) * (n) / (n - 1)
  deltaSE <- sqrt(4 * (1 - 2 * yBar)^2 * yBar * (1 - yBar) * n^2 / (n - 1)^3)
  c(lrAna, deltaSE)
}
#' Find variance of HSS reference loss through the delta nethod
#'
#' @returns The approximated variance
#' @inheritParams lrAnaBinMod
getLrSE = function(yBar, kappaHat, n, covEst){
    vec = c(-2*kappaHat+1, -2*yBar+1)
    vcovMat = matrix(c(yBar*(1-yBar)/(n-1), covEst["cov"], covEst["cov"], covEst["varKappa"]), 2, 2)
    sqrt(vec %*% vcovMat %*% vec)
}
