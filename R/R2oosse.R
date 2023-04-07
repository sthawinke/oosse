#' Estimate the out-of-sample R² and its standard error
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
#' @param ... passed onto fitFun and predFun
#'
#' @return A list with components
#' \item{R2}{Estimate of the R² with standard error}
#' \item{MSE}{Estimate of the MSE with standard error}
#' \item{MST}{Estimate of the MST with standard error}
#' \item{corMSEMST}{Estimated correlation between MSE and MST estimators}
#' \item{params}{List of parameters used}
#' \item{fullModel}{The model trained on the entire dataset using fitFun}
#' @export
#' @importFrom methods formalArgs
#' @importFrom stats cor sd var
#' @importFrom BiocParallel bplapply multicoreWorkers
#'
#' @details Implements the calculation of the R² and its standard error by \insertCite{Hawinkel2023}{oosse}.
#'  Multithreading is used as provided by the BiocParallel package,
#' A rough estimate of expected computation time is printed when prinTimeEstimate is true, but this is purely indicative.
#' The options to estimate the mean squared error (MSE) are cross-validation \insertCite{Bates2021}{oosse} or the .632 bootstrap \insertCite{Efron1997}{oosse}.
#' @examples
#' data(Brassica)
#' #Linear model
#' fitFunLM = function(y, x){lm.fit(y = y, x = cbind(1, x))}
#' predFunLM = function(mod, x) {cbind(1,x) %*% mod$coef}
#' y = Brassica$Pheno$Leaf_8_width
#' R2lm = R2oosse(y = Brassica$Pheno$Leaf_8_width, x = Brassica$Expr[, 1:10],
#' fitFun = fitFunLM, predFun = predFunLM, nFolds = 5)
#' # A higher number of folds (e.g. 10) is recommended if computational resources allow
#' @seealso \link{buildConfInt}
#' @importFrom Rdpack reprompt
#' @references
#'   \insertAllCited{}
R2oosse = function(y, x, fitFun, predFun, methodMSE = c("CV", "bootstrap"), methodCor = c("nonparametric", "jackknife"), printTimeEstimate = TRUE,
                       nFolds = 10L, nInnerFolds = nFolds - 1L, cvReps = 200L, nBootstraps = 200L, nBootstrapsCor = 50L,...){
    fitFun = checkFitFun(fitFun) #Version of the fit function for internal use
    predFun = checkPredFun(predFun)
    methodMSE = match.arg(methodMSE)
    methodCor = match.arg(methodCor)
    if((n <- length(y)) != NROW(x)){
        stop("Number of observations in y and x must match!")
    }
    if(NCOL(y)!=1){
        stop("Outcome must be one-dimensional!")
    }
    stopifnot(is.numeric(nFolds), is.numeric(nInnerFolds), is.numeric(cvReps), is.numeric(nBootstraps), is.numeric(nBootstrapsCor))
    if(cvReps < 1e2){
        warning("Fewer than 100 repeats of the cross-validation split does not yield reliable estimates of the standard error!",
                immediate. = TRUE)
    }
    singleRunTime = system.time((predFun(fullModel <- fitFun(y, x, ...), x)-y)^2)["elapsed"]
    if(printTimeEstimate){
        #Predict time this will take
        estMSEreps = switch(methodMSE, "CV" = cvReps*nFolds*(nInnerFolds+1),
                            "bootstrap" = nBootstraps*2)
        # Number of repeats for estimating the MSE and its SE
        estCorReps = switch(methodCor, "nonparametric" = nBootstrapsCor, "jackknife" = n)*
            switch(methodMSE, "CV" = nFolds, "bootstrap" = nBootstraps) #Number of repeats for correlation estimation
        message("Fitting and evaluating the model once took ", formatSeconds(singleRunTime), ".\nYou requested ",
            switch(methodMSE,
                   "CV" = paste0(cvReps, " repeats of ", nFolds, "-fold cross-validation"),
                   "bootstrap" = paste(nBootstraps, ".632 bootstrap instances")),
        " with ", nCores <- multicoreWorkers(), " cores, which is expected to last for roughly\n",
        formatSeconds(sec <- (estMSEreps + estCorReps)*singleRunTime/nCores),
        if(nCores==1 && (sec >10)) "\nConsider using multithreading with the 'BiocParallel' package to speed up computations.")
    }
    seVec = estMSE(y, x, fitFun, predFun, methodMSE, nFolds = nFolds, nInnerFolds = nInnerFolds, cvReps = cvReps, nBootstraps = nBootstraps)
    corMSEMST = estCorMSEMST(y, x, fitFun, predFun, methodMSE, methodCor, nBootstrapsCor, nFolds = nFolds, nBootstraps = nBootstraps)
    R2est = RsquaredSE(MSE = seVec["MSE"], margVar = margVar <- var(y), n = n, SEMSE = seVec["MSESE"], corMSEMST = corMSEMST)
    MST = margVar*(n+1)/n
    return(list("R2" = R2est, "MSE" = seVec, "MST" = c("MST" = MST, "MSTSE" = sqrt(2/(n-1))*MST), "corMSEMST" = corMSEMST,
         "params" = c(switch(methodMSE,
                             "CV" = c("nFolds" = nFolds, "nInnerFolds" = nInnerFolds, "cvReps" = cvReps),
                             "bootstrap" = c("nBootstraps" = nBootstraps)), "methodMSE" = methodMSE,
                      "methodCor" = methodCor, "nBootstrapsCor" = if(methodCor=="nonparametric") nBootstrapsCor),
         "fullModel" = fullModel))
}
