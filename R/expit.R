#' The expit function
#'
#' @param x
#' @return expit(x)
expit = function(x) {
    exp(x)/(1+exp(x))
}