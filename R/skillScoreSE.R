#' Calculate out-of-sample skill score and its standard error based on MSE estimates
#'
#' @param meanLoss An estimate of the mean squared error (MSE)
#' @param meanLossSE The standard error on the MSE estimate
#' @param n Sample size, needed for the R^2
#' @param corEst The correlation between model and reference loss estimators, or a 3x3 correlation matrix for the Peirce skill score
#' @param refLoss,refLossSE The reference loss and its standard error.
#' @inheritParams oosse
#'
#' @details This function is exported to allow the user to estimate the MSE and its standard error
#' and the correlation between MSE and refLoss estimators himself.
#' @return A vector with the skill score and standard error estimates
#' @importFrom Matrix nearPD
#' @export
#' @examples
#' # The out-of-sample R² calculated using externally provided estimates
#' skillScoreSE(
#'   meanLoss = 3, refLoss = 4, refLossSE = 0.24, meanLossSE = 0.4,
#'   corEst = 0.75, skillScore = "R2"
#' )
#' # The out-of-sample Brier skill score
#' skillScoreSE(
#'   meanLoss = .3, meanLossSE = 0.4, refLoss = .4, refLossSE = 0.2,
#'   corEst = 0.75, skillScore = "Brier"
#' )
#' # The out-of-sample Peirce skill score
#' skillScoreSE(
#'   meanLoss = .3, meanLossSE = 0.05, refLoss = .44,
#'   refLossSE = 0.02, corEst = 0.75, skillScore = "Heidke"
#' )
#' @seealso \link{oosse}
#' @references
#'     \insertRef{Hawinkel2023}{oosse}
skillScoreSE <- function(meanLoss, meanLossSE, n, corEst, refLoss,
                         refLossSE, skillScore) {
  skillScore <- match.arg(skillScore, choices = as.character(formals(oosse)$skillScore)[-1])
  stopifnot(
    all(corEst >= -1), all(corEst <= 1), meanLoss > 0 || skillScore == "McFadden",
    meanLossSE > 0, all(refLoss > 0) || skillScore == "McFadden",
    skillScore == "R2" || refLossSE >0,
    length(corEst) == 1 || skillScore == "Peirce"
  )
  ss <- unname(1 - meanLoss / refLoss) # The skill score estimate
  if (missing(refLossSE) && skillScore == "R2") {
    refLossSE <- sqrt(2 / (n - 1)) * refLoss # The standard error on the MST
  }
  if (skillScore == "Peirce") {
    Grad <- c(-1 / refLoss["Estimate"], 1 / refLoss["Estimate"], (meanLoss - refLoss["EstimateModel"]) / refLoss["Estimate"]^2) # The gradient
    covMat <- tcrossprod(c(meanLossSE, refLossSE["Estimate"], refLossSE["EstimateModel"])) * corEst
  } else {
    Grad <- c(-1 / refLoss, meanLoss / refLoss^2) # The gradient
    covSSEmarg <- corEst * meanLossSE * refLossSE # Covariance between meanLoss and refLoss estimates
    covMat <- matrix(c(meanLossSE^2, covSSEmarg, covSSEmarg, refLossSE^2), 2, 2) # The covariance matrix
  }
  if (!isPD(covMat)) {
    covMat <- nearPD(covMat)$mat # Convert to nearest positive definite matrix
  }
  out <- c(ss, as.vector(sqrt(Grad %*% covMat %*% Grad)))
  names(out) <- c("Estimate", "StandardError")
  return(out)
}
