#' The expit function
#'
#' @param x The input to be converted to a probability
#' @return expit(x)
expit = function(x) {
    exp(x)/(1+exp(x))
}