#' Calculate the out-of-sample R² and its standard error based on MSE estimates
#'
#' @param MSE An estimate of the mean squared error (MSE)
#' @param margVar The marginal variance of the outcome, not scaled by (n+1)/n
#' @param SEMSE The standard error on the MSE estimate
#' @param n the sample size of the training data
#' @param corMSEMST The correlation between MSE and marginal variance estimates
#'
#' @details This function is exported to allow the user to estimate the MSE and its standard error
#' and the correlation between MSE and MST estimators himself.
#' The marginal variance is scaled by (n+1)/n to the out-of-sample MST, so the user does not need to do this.
#' @return A vector with the R² and standard error estimates
#' @importFrom Matrix nearPD
#' @export
#' @examples
#' #The out-of-sample R² calculated using externally provided estimates
#' RsquaredSE(MSE = 3, margVar = 4, SEMSE = 0.4, n = 50, corMSEMST = 0.75)
#' @seealso \link{R2oosse}
#' @references
#'     \insertRef{Hawinkel2023}{oosse}
RsquaredSE = function(MSE, margVar, SEMSE, n, corMSEMST){
    stopifnot(corMSEMST >= -1, corMSEMST <=1, MSE > 0, margVar > 0, n > 1, SEMSE > 0)
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
