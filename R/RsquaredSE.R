#' Calculate the out-of-sample R² and its standard error
#'
#' @param MSE An estimate of the mean squared error (MSE)
#' @param margVar The marginal variance of the outcome
#' @param SEMSE The standard error on the MSE estimate
#' @param n the sample size of the training data
#' @param corMSEMST The correlation between MSE and marginal variance estimates
#'
#' @details The marginal variance is scaled by (n+1)/n to the out-of-sample MST, so the user does not need to do this
#' @return A vector with the R² and standard error estimates
#' @importFrom Matrix nearPD
#' @export
#' @examples
#' The out-of-sample R² calculated using externally provided estimates
#' Rsquared(MSE = 3, margVar = 4, SEMSE = 0.4, n = 50, corMSEMST = 0.75)
RsquaredSE = function(MSE, margVar, SEMSE, n, corMSEMST){
    MST = margVar*(n+1)/n #Inflate marginal variance to out-of-sample MST
    Grad = c(-1/MST, MSE/MST^2) #The gradient
    SEmargVar = sqrt(2/(n-1))*MST #The standard error on the MST
    covSSEmarg = corMSEMST*SEMSE*SEmargVar #Covariance between MSE and MST estimates
    covMat = matrix(c(SEMSE^2, covSSEmarg, covSSEmarg, SEmargVar^2), 2, 2) #The covariance matrix
    if(!isPD(covMat)){
        covMat = nearPD(covMat)$mat #Convert to nearest positive definite matrix
    }
    c("R2" = unname(1-MSE/MST), "R2SE" = as.vector(sqrt(Grad %*% covMat %*% Grad)))
}
