#' Helper function to check if matrix is positive definite
#'
#' @param mat The matrix
#' @param tol The tolerance
#'
#' @return A boolean indicating positive definiteness
isPD <- function(mat, tol = 1e-6) {
  ev <- eigen(mat, symmetric = TRUE)$values
  all(ev >= -tol * abs(ev[1L]))
}
#' The expit function
#'
#' @param x The input to be converted to a probability
#' @return expit(x)
#' @export
#' @examples
#' expit(2)
expit <- function(x) {
  exp(x) / (1 + exp(x))
}
#' Determine the skill score
#'
#' @inheritParams oosse
#' @return The name of the loss
determineLoss <- function(skillScore) {
  if (skillScore %in% c("R2", "Brier", "RankedProbability")) {
    "squared"
  } else if (skillScore %in% c("Peirce", "Appleman", "Heidke")) {
    "binary"
  } else if (skillScore %in% "McFadden") {
    "logistic"
  }
}
#' Determine the names for skill score and model and reference loss
#'
#' @inheritParams oosse
#' @return A character vector of length 3
determineNames <- function(skillScore) {
  switch(skillScore,
    "R2" = c("R2", "MSE", "MST"),
    "Brier" = c("BrierSkillScore", "BrierScore", "ReferenceBrierScore"),
    "Peirce" = c("PeirceSkillScore", "ModelMisclassRate", "ReferenceMisclassRate"),
    "Heidke" = c("HeidkeSkillScore", "ModelMisclassRate", "ReferenceMisclassRate"),
    "Appleman" = c("ApplemanSkillScore", "ModelMisclassRate", "ReferenceMisclassRate"),
    "McFadden" = c("McFaddenSkillScore", "ModelLogLoss", "ReferenceLogLoss"),
    "RankedProbability" = c("RankedProbabilitySkillScore", "ModelSquaredLoss", "ReferenceSquaredLoss")
  )
}
#' Estimate the time needed to finish and print message
#'
#' @param singleRunTime The time needed to fit and evaluate the prediction model once
#' @param n The sample size
#' @importFrom BiocParallel bpnworkers bpparam
#' @inheritParams oosse
#' @return Prints a message to the console
timeEstimate <- function(methodLoss, cvReps, nFolds, nInnerFolds, nBootstraps,
                         nBootstrapsCor, singleRunTime, n, methodCor) {
  # Predict time this will take
  estModelLossreps <- switch(methodLoss,
    "CV" = cvReps * nFolds * (nInnerFolds + 1),
    "bootstrap" = nBootstraps * 2
  )
  # Number of repeats for estimating the MSE and its SE
  estCorReps <- switch(methodCor,
    "nonparametric" = nBootstrapsCor,
    "jackknife" = n
  ) *
    switch(methodLoss,
      "CV" = nFolds,
      "bootstrap" = nBootstraps
    ) # Number of repeats for correlation estimation
  message(
    "Fitting and evaluating the model once took ", formatSeconds(singleRunTime), ".\nYou requested ",
    switch(methodLoss,
      "CV" = paste0(cvReps, " repeats of ", nFolds, "-fold cross-validation"),
      "bootstrap" = paste(nBootstraps, ".632 bootstrap instances")
    ),
    " with ", nCores <- bpnworkers(bpparam()), " cores, which is expected to last for roughly\n",
    formatSeconds(sec <- (estModelLossreps + estCorReps) * singleRunTime / nCores),
    if (nCores == 1 && (sec > 10)) {
      "\nConsider using multithreading with the 'BiocParallel' package to speed up computations."
    }, "\n"
  )
}
#' Construct matrix form of multivariate outcome
#'
#' @param y The vector with more than 2 unique values
#'
#' @return A matrix with categories in the columns and number of rows equal to the length of y
#' @importFrom stats model.matrix
makeYMatrix <- function(y) {
  model.matrix(~ y - 1)
}
#' Subset y regardless of whether it is a vector or matrix
#'
#' @param y The vector
#' @param yMat The matrix
#' @param id The subsetting index
#' @inheritParams oosse
#'
#' @return The subset vector or matrix
subsetY <- function(y, yMat, skillScore, id) {
  if (skillScore == "RankedProbability") {
    yMat[id, , drop = FALSE]
  } else {
    y[id]
  }
}
#' Build the multinomial variance covariance matrix
#'
#' @param pi The vector of probabilities
#' @param n The sample size
#'
#' @return The variance covariance matrix
buildVarCovarMult <- function(pi, n) {
  tmp <- tcrossprod(pi) / (1 - n)
  diag(tmp) <- pi * (1 - pi) / (n - 1)
  tmp
}
