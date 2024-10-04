#' Estimate out-of-sample R² and its standard error
#'
#' @param y The vector of outcome values
#' @param x The matrix of predictors
#' @param fitFun The function for fitting the prediction model
#' @param predFun The function for evaluating the prediction model
#' @param methodLoss The method to estimate the MSE, either "CV" for cross-validation or "bootstrap" for .632 bootstrap
#' @param methodCor The method to estimate the correlation between MSE and MST estimators, either "nonparametric" or "jackknife"
#' @param printTimeEstimate A boolean, should an estimate of the running time be printed?
#' @param nFolds The number of outer folds for cross-validation
#' @param nInnerFolds The number of inner cross-validation folds
#' @param cvReps The number of repeats for the cross-validation
#' @param nBootstraps The number of .632 bootstraps
#' @param nBootstrapsCor The number of bootstraps to estimate the correlation
#' @param skillScore The desired skill score. Currently, "R2", "Brier" and "Heidke" are implemented.
#' @param estCovMethod The method to estimate the covariance between estimators of success probability and
#' the chance of this probability falling below 0.5
#' @param ... passed onto fitFun and predFun
#'
#' @return A list with components
#' \item{R2}{Estimate of the R² with standard error}
#' \item{MSE}{Estimate of the MSE with standard error}
#' \item{MST}{Estimate of the MST with standard error}
#' \item{corMSEMST}{Estimated correlation between MSE and MST estimators}
#' \item{params}{List of parameters used}
#' \item{fullModel}{The model trained on the entire dataset using fitFun}
#' \item{n}{The sample size of the training data}
#' @export
#' @import BiocParallel
#' @importFrom methods formalArgs
#' @importFrom stats cor sd var
#' @importFrom doParallel registerDoParallel
#' @importFrom Rdpack reprompt
#'
#' @details Implements the calculation of the R² and its standard error by \insertCite{Hawinkel2023}{oosse}.
#' Multithreading is used as provided by the BiocParallel or doParallel packages,
#' A rough estimate of expected computation time is printed when printTimeEstimate is true, but this is purely indicative.
#' The options to estimate the mean squared error (MSE) are cross-validation \insertCite{Bates2023}{oosse} or the .632 bootstrap \insertCite{Efron1997}{oosse}.
#' @examples
#' data(Brassica)
#' #Linear model
#' fitFunLM = function(y, x){lm.fit(y = y, x = cbind(1, x))}
#' predFunLM = function(mod, x) {cbind(1,x) %*% mod$coef}
#' y = Brassica$Pheno$Leaf_8_width
#' R2lm = oosse(y = Brassica$Pheno$Leaf_8_width, x = Brassica$Expr[, 1:10],
#' fitFun = fitFunLM, predFun = predFunLM, nFolds = 10, skillScore = "R2")
#' @seealso \link{buildConfInt}
#' @references
#'   \insertAllCited{}
oosse = function(y, x, fitFun, predFun,  skillScore = c("R2", "Brier", "Heidke"), methodLoss = c("CV", "bootstrap"), methodCor = c("nonparametric", "jackknife"), printTimeEstimate = TRUE,
                       nFolds = 10L, nInnerFolds = nFolds - 1L, cvReps = 200L, nBootstraps = 200L, nBootstrapsCor = 50L,
                  estCovMethod = c("analytical", "bootstrap"), ...){
    fitFun = checkFitFun(fitFun) #Version of the fit function for internal use
    predFun = checkPredFun(predFun)
    methodLoss = match.arg(methodLoss)
    methodCor = match.arg(methodCor)
    skillScore = match.arg(skillScore)
    estCovMethod = match.arg(estCovMethod)
    loss = if(skillScore %in% c("R2", "Brier")){
        "squared"
    } else if(skillScore %in% c("Heidke")){
        "binary"
    }
    if(skillScore %in% c("Brier", "Heidke") && !all(y %in% c(0,1))){
        stop("For skill score", skillScore, "only binary outcomes y are allowed!")
    }
    if(is.data.frame(x)){
        stop("Supplying dataframes as predictors is not supported. Convert to a design matrix using model.matrix.\nSee the vignette for an example.")
    }
    if((n <- length(y)) != NROW(x)){
        stop("Number of observations in y and x must match!")
    } else if(NCOL(x) == 1){
        x = matrix(x, nrow = n) #Convert to matrix if vector supplied
    }
    if(NCOL(y)!=1){
        stop("Outcome must be one-dimensional!")
    }
    if(nFolds < 3){
        stop("Number of folds must be at least 3!")
        }
    stopifnot(is.numeric(nFolds), is.numeric(nInnerFolds), is.numeric(cvReps), is.numeric(nBootstraps), is.numeric(nBootstrapsCor))
    if(cvReps < 1e2){
        warning("Fewer than 100 repeats of the cross-validation split does not yield reliable estimates of the standard error!",
                immediate. = TRUE)
    }
    singleRunTime = system.time(fullPred <- try(predFun(fullModel <- try(fitFun(y, x, ...), silent = TRUE), x), silent = TRUE))["elapsed"]
    if(inherits(fullModel, "try-error")){
        stop("Fitting model failed with error", fullModel, "\nCheck your fitFun")
    } else if (inherits(fullPred, "try-error")){
        stop("Prediction model failed with error", fullPred, "\nCheck your predFun")
    } else if(printTimeEstimate){
        #Predict time this will take
        estModelLossreps = switch(methodLoss, "CV" = cvReps*nFolds*(nInnerFolds+1),
                            "bootstrap" = nBootstraps*2)
        # Number of repeats for estimating the MSE and its SE
        estCorReps = switch(methodCor, "nonparametric" = nBootstrapsCor, "jackknife" = n)*
            switch(methodLoss, "CV" = nFolds, "bootstrap" = nBootstraps) #Number of repeats for correlation estimation
        message("Fitting and evaluating the model once took ", formatSeconds(singleRunTime), ".\nYou requested ",
            switch(methodLoss,
                   "CV" = paste0(cvReps, " repeats of ", nFolds, "-fold cross-validation"),
                   "bootstrap" = paste(nBootstraps, ".632 bootstrap instances")),
        " with ", nCores <-  bpnworkers(bpparam()), " cores, which is expected to last for roughly\n",
        formatSeconds(sec <- (estModelLossreps + estCorReps)*singleRunTime/nCores),
        if(nCores==1 && (sec >10)) {"\nConsider using multithreading with the 'BiocParallel' package to speed up computations."}, "\n")
    }
    modelLoss = estModelLoss(y, x, fitFun, predFun, methodLoss, nFolds = nFolds,
                             nInnerFolds = nInnerFolds, cvReps = cvReps, nBootstraps = nBootstraps, loss = loss)
    refLoss = estRefLoss(y, x, skillScore = skillScore, margVar = margVar <- var(y), nBootstraps = nBootstraps, estCovMethod = estCovMethod)
    corEst = estCorMeanRef(y, x, fitFun, predFun, methodLoss, methodCor, nBootstrapsCor, nFolds = nFolds, nBootstraps = nBootstraps, loss = loss)
    skillScoreRes = skillScoreSE(meanLoss = modelLoss["Estimate"], margVar = margVar, n = n,
                              meanLossSE = modelLoss["StandardError"], corEst = corEst, refLoss = refLoss["Estimate"], refLossSE = refLoss["StandardError"])
    list0 = list(skillScoreRes, modelLoss, refLoss)
    names(list0) = switch(skillScore,
                          "R2" = c("R2", "MSE", "MST"),
                          "Brier" = c("Brier skill score", "Brier score", "Reference loss"),
                          "Heidke" = c("Heidke skill score", "Model missclassification rate", "Reference missclassification rate"))
    return(c(list0, list("corEst" = corEst,
         "params" = c(switch(methodLoss,
                             "CV" = c("nFolds" = nFolds, "nInnerFolds" = nInnerFolds, "cvReps" = cvReps),
                             "bootstrap" = c("nBootstraps" = nBootstraps)), "methodLoss" = methodLoss, "loss" = loss, "skillScore" = skillScore,
                      "methodCor" = methodCor, "nBootstrapsCor" = if(methodCor=="nonparametric") nBootstrapsCor),
         "fullModel" = fullModel, "n" = n)))
}
