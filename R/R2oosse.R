#' Estimate the out-of-sample R squared and its standard error
#'
#' @param y The vector of outcome values
#' @param x The matrix of predictors
#' @param fitFun The function for fitting the prediction model
#' @param predFun The function for evaluating the prediction model
#' @param methodMSE The method to estimate the MSE, either "CV" for cross-validation or "bootstrap" for .632 bootstrap
#' @param methodCor The method to estimate the correlation between MSE and MST estimators, either "nonparametric" or "jackknife"
#' @param printTimeEstimate A boolean, should an estimate of the running time be printed?
#' @param nFolds The number of outer folds for cross-validation
#' @param nInnerFolds The number of inner cross-validation folds
#' @param cvReps The number of repeats for the cross-validation
#' @param nBootstraps The number of .632 bootstraps
#' @param nBootstrapsCor The number of bootstraps to estimate the correlation
#' @param ... passed onto fitFun
#'
#' @return A list with components
#' \item{R2}{Estimate of the R² with standard error}
#' \item{MSE}{Estimate of the MSE with standard error}
#' \item{MST}{Estimate of the MST with standard error}
#' \item{corMSEMST}{Estimated correlatio between MSE and MST estimators}
#' \item{params}{List of parameters used}
#' \item{y,x,fitFun,predFun}{Arguments as provided}
#' @export
#' @importFrom methods formalArgs
#' @importFrom stats cor sd var
#' @importFrom BiocParallel bplapply multicoreWorkers
#'
#' @examples
#' n = 40;p=3
#' y = rnorm(n)
#' x = matrix(rnorm(n*p),n,p)
#' fitFun = function(y, x, id){lm.fit(y = y[id], x = cbind(1, x[id,]))}
#' predFun = function(mod, x) {cbind(1,x) %*% mod$coef}
#' R2oosse(y = y, x = x, predFun = predFun, fitFun = fitFun)
R2oosse = function(y, x, fitFun, predFun, methodMSE = c("CV", "bootstrap"), methodCor = c("nonparametric", "jackknife"), printTimeEstimate = TRUE,
                       nFolds = 10, nInnerFolds = nFolds - 1, cvReps = 200, nBootstraps = 200, nBootstrapsCor = 50, ...){
    fitFun = checkFitFun(fitFun)
    methodMSE = match.arg(methodMSE)
    methodCor = match.arg(methodCor)
    if((n <- length(y)) != NROW(x)){
        stop("Number of observations in y and x must match!")
    }
    if(NCOL(y)!=1){
        stop("Outcome must be one-dimesnional!")
    }
    if(cvReps < 1e2){
        warning("Fewer than 100 repeats of the cross-validation split does not yield reliable estimates of the standard error!",
                immediate. = TRUE)
    }

    #Predict time this will take
    singleRunTime = system.time(predFun(fullModel <- fitFun(y, x, id = seq_len(n), ...), x))["elapsed"]
    cat("Fitting and evaluating the model once took", formatSeconds(singleRunTime), ".\nYou requested",
        switch(methodMSE,
               "CV" = paste0(cvReps, " repeats of ", nFolds, "-fold cross-validation"),
               "bootstrap" = paste(nBootstraps, ".632 bootstrap instances")),
    "with", nCores <- multicoreWorkers(), "cores, which is expected to last for\n",
    formatSeconds((switch(methodMSE, "CV" = cvReps*nFolds, "bootstraps" = nBootstraps) +
                       switch(methodCor, "nonparametric" = nBootstrapsCor, "jackknife" = n))*singleRunTime/nCores))

    seVec = estMSE(y, x, fitFun, predFun, methodMSE, nFolds = nFolds, nInnerFolds = nInnerFolds, cvReps = cvReps, nBootstraps = nBootstraps)
    corMSEMST = estCorMSEMST(y, x, fitFun, predFun, methodMSE, methodCor, nBootstrapsCor, nFolds = nFolds)
    R2est = RsquaredSE(MSE = seVec["MSE"], margVar = margVar <- var(y), n = n, SEMSE = seVec["MSESE"], corMSEMST = corMSEMST)
    MST = margVar*(n+1)/n
    return(list("R2" = R2est, "MSE" = seVec, "MST" = c("MST" = MST, "MSTSE" = sqrt(2/(n-1))*MST), "corMSEMST" = corMSEMST,
         "params" = c(switch(methodMSE,
                             "CV" = c("nFolds" = nFolds, "nInnerFolds" = nInnerFolds, "cvReps" = cvReps),
                             "bootstrap" = c("nBootstraps" = nBootstraps)), "methodCor" = methodCor,
                         "methodMSE" = methodMSE, "nBootstrapsCor" = if(methodCor=="nonparametric") 50),
         "y" = y, "x" = x, "fitFun" = fitFun, "predFun" = predFun))
}
