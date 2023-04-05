#' Estimate the MSE and its standard error
#'
#' @param y The vector of outcome values
#' @param x The matrix of predictors
#' @param fitFun The function for fitting the predicition model
#' @param predFun The function for evaluating the prediction model
#' @param methodMSE The method to estimate the MSE, either "CV" for cross-validation or "bootstrap" for .632 bootstrap
#' @param methodCor The method to estimate the correlation between MSE and MST estimators, either "nonparametric" or "jackknife"
#' @param nFolds The number of outer folds for cross-validation
#' @param nInnerFolds The number of inner cross-validation folds
#' @param cvReps The number of repeats for the cross-validation
#' @param nBootstraps
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
estMSEandSE = function(y, x, fitFun, predFun, methodMSE = c("CV", "bootstrap"), methodCor = c("nonparametric", "jackknife"),
                       nFolds = 10, nInnerFolds = nFolds - 1, cvReps = 200, nBootstraps = 200, nInnerBootstraps = 200, nBootstrapsCor = 50, ...){
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
    singleRunTime = system.time(predFun(fullModel <- fitFun(y, x, id = seq_len(n), ...), newx = x))
    cat("Fitting and evaluating the model once took", formatSeconds(singleRunTime), ".\nYou requested",
        switch(methodMSE,
               "CV" = paste0(cvReps, " repeats of ", nFolds, "-fold cross-validation"),
               "bootstrap" = paste(nBootstraps*nInnerBootstraps, ".632 bootstrap instances")),
    "with", nCores <- multicoreWorkers(), "cores, which is expected to last for\n",
    formatSeconds((switch(methodMSE, "CV" = cvReps*nFolds, "bootstraps" = nBootstraps) +
                       switch(methodCor, "nonparametric" = nBootstrapsCor, "jackknife" = n))*singleRunTime/nCores))

    #Estimate MSE and its standard error
    if(methodMSE == "CV"){
        #Nested cross-validation
        unFolds <- seq_len(nFolds);unFoldsIn <- seq_len(nInnerFolds) #Prepare the folds
        cvSplitReps = bplapply(seq_len(cvReps), function(cvs){
            folds = sample(rep(unFolds, length.out = n))
            lapply(unFolds, function(uf){
                #Outer loop
                idTrain = folds!=uf
                modTrain = fitFun(y, x, idTrain)
                predTest = predFun(modTrain, x[!idTrain,])
                eOut = (predTest-y[!idTrain])^2
                #Inner loop
                inFolds = sample(rep(unFoldsIn, length.out = sum(idTrain)))
                eIn =  lapply(unFoldsIn, function(inf){
                    idTrainIn = inFolds!=inf
                    modTrain = fitFun(y, x, idTrainIn)
                    predTest = predFun(modTrain, x[!idTrainIn])
                    (predTest-y[!idTrainIn])^2
                })
                #summary statistics
                errHatTilde = mean(unlist(eIn))
                a = (errHatTilde-mean(eOut))^2
                b = var(eOut)/length(eOut)
                list("a" = a, "b" = b, "errHatTilde" = errHatTilde, "eOut" = eOut)
            })
        })
        seVec = getSEsNested(cvSplitReps, nFolds, n)
    } else if(methodMSE == "bootstrap") {
        bootReps = bplapply(seq_len(nBootstraps), function(br){
            id = sample(n, replace = TRUE)
            #.632 bootstrap
            MSE632est = boot632(y, x, id, fitFun, predFun)
            #Out of bag bootstrap
            oob = bootOob(y, x, id, seq_along(y), fitFun, predFun)
            list("oobObj" = oob, "MSE632est" = MSE632est)
        })
        MSE632est = mean(vapply(FUN.VALUE = double(1), bootReps, function(x) {x$MSE632est}))
        MSEoob = processOob(bootReps)
        SEmse = MSE632est/MSEoob["MSEhat"]*MSEoob["SEhat"]
        seVec = c("MSEhat" = MSE632est, "SEhat" = SEmse)
    }
    corMSEMST = estCorMSEMST(y, x, fitFun, predFun, methodMSE, methodCor, nBootstrapsCor)
}
n = 40;p=3
y = rnorm(n)
x = matrix(rnorm(n*p),n,p)
fitFun = function(y, x, id){lm.fit(y = y[id], x = cbind(1, x[id,]))}
predFun = function(mod, x) {cbind(1,x) %*% mod$coef}
