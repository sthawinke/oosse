#' Calculate out-of-sample R² and its standard error based on MSE estimates
#'
#' @param meanLoss An estimate of the mean squared error (MSE)
#' @param margVar The marginal variance of the outcome, not scaled by (n+1)/n
#' @param meanLossSE The standard error on the MSE estimate
#' @param n the sample size of the training data
#' @param corEst The correlation between model and reference loss estimators
#' @param refLoss,refLossSE The reference loss and its standard error. Not required when skillScore is "R2"
#' @inheritParams oosse
#'
#' @details This function is exported to allow the user to estimate the MSE and its standard error
#' and the correlation between MSE and refLoss estimators himself.
#' The marginal variance is scaled by (n+1)/n to the out-of-sample refLoss, so the user does not need to do this.
#' @return A vector with the R² and standard error estimates
#' @importFrom Matrix nearPD
#' @export
#' @examples
#' # The out-of-sample R² calculated using externally provided estimates
#' skillScoreSE(meanLoss = 3, margVar = 4, meanLossSE = 0.4, n = 50,
#' corEst = 0.75)
#' # The out-of-sample Brier skill score
#' skillScoreSE(meanLoss = 3, meanLossSE = 0.4, refLoss = 4, refLossSE = 0.2,
#' corEst = 0.75, skillScore = "Brier")
#' # The out-of-sample Heidke skill score
#' skillScoreSE(meanLoss = .3, meanLossSE = 0.05, refLoss = .44,
#' refLossSE = 0.02, corEst = 0.75, skillScore = "Heidke")
#' @seealso \link{oosse}
#' @references
#'     \insertRef{Hawinkel2023}{oosse}
skillScoreSE = function(meanLoss, meanLossSE, margVar, n, corEst, refLoss, refLossSE, skillScore = c("R2", "Brier", "Heidke")){
    stopifnot(corEst >= -1, corEst <=1, meanLoss > 0, missing(margVar) || margVar > 0,
              missing(n) || n > 1, meanLossSE > 0, skillScore == "R2" || (meanLoss < 1 && refLoss < 1))
    skillScore = match.arg(skillScore)
    if(skillScore == "R2"){
        refLoss = margVar*(n+1)/n #Inflate marginal variance to out-of-sample MST
        refLossSE = sqrt(2/(n-1))*refLoss #The standard error on the MST
    } else if(missing(refLoss) || missing(refLossSE)){
        stop("Reference loss and its variance must be provided when skill score is not R2!")
    }
    Grad = c(-1/refLoss, meanLoss/refLoss^2) #The gradient
    covSSEmarg = corEst*meanLossSE*refLossSE #Covariance between meanLoss and refLoss estimates
    covMat = matrix(c(meanLossSE^2, covSSEmarg, covSSEmarg, refLossSE^2), 2, 2) #The covariance matrix
    if(!isPD(covMat)){
        covMat = nearPD(covMat)$mat #Convert to nearest positive definite matrix
    }
    out = c(unname(1-meanLoss/refLoss), as.vector(sqrt(Grad %*% covMat %*% Grad)))
    names(out) = c("Estimate", "StandardError")
    return(out)
}
