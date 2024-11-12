#' Analytically estimate out-of-sample binary reference loss and its standard error
#' for the Heidke and Peirce skill scores.
#'
#' @param yBar The average outcome
#' @param kappaHat The average prediction
#' @param n The sample size
#' @param covEst The estimated covariance between estimators for yBar and kappaHat
#'
#' @return The
lrAnaBinMod = function(yBar, kappaHat, n, covEst){
    lrAna = yBar*(1-kappaHat) + (1-yBar)*kappaHat + 2 *covEst
    deltaSE = NA # Fix me
    c(lrAna, deltaSE)
    #ADD: standard error, see Hogan 2009
}
lrAnaBinObs = function(yBar, n){
    lrAna = 2*yBar*(1-yBar)*(n)/(n-1)
    deltaSE = sqrt(4*(1-2*yBar)^2*yBar*(1-yBar)*n^2/(n-1)^3)
    c(lrAna, deltaSE)
}