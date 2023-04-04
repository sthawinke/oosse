#' Estimate the MSE and its standard error
#'
#' @param y The vector of outcome values
#' @param x The matrix of predictors
#' @param predFun The function for fitting the predicition model
#' @param evalPredFun The function for evaluating the prediction model
#' @param methodMSE The method to estimate the MSE, either "CV" for cross-validation or "bootstrap" for .632 bootstrap
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
estMSEandSE = function(y, x, predFun, evalPredFun, methodMSE = c("CV", "bootstrap"),
                       nFolds = 10, nInnerFolds = nFolds - 1, cvReps = 200, nBootstraps = 200, nInnerBootstraps = 200, ...){
    predFun = checkPredFun(predFun)
    methodMSE = match.arg(methodMSE)
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
    singleRunTime = system.time(evalPredFun(fullModel <- predFun(y, x, id = seq_len(n), ...), newx = x))
    cat("Fitting and evaluating the model once took", formatSeconds(singleRunTime), ".\nYou requested",
        switch(methodMSE,
               "CV" = paste0(cvReps, " repeats of ", nFolds, "-fold cross-validation"),
               "bootstrap" = paste(nBootstraps*nInnerBootstraps, ".632 bootstrap instances")),
    "with", nCores <- multicoreWorkers(), "cores, which is expected to last for\n",
    formatSeconds(singleRunTime/nCores * switch(methodMSE, "CV" = cvReps*nFolds, "bootstraps" = nBootstraps)))

    if(methodMSE == "CV"){
        #Nested cross-validation
        unFolds <- seq_len(nFolds);unFoldsIn <- seq_len(nInnerFolds) #Prepare the folds
        cvSplitReps = bplapply(seq_len(cvReps), function(cvs){
            folds = sample(rep(unFolds, length.out = n))
            lapply(unFolds, function(uf){
                #Outer loop
                idTrain = folds!=uf
                modTrain = predFun(y, x, idTrain)
                predTest = evalPredFun(modTrain, x[!idTrain,])
                eOut = (predTest-y[!idTrain])^2
                #Inner loop
                inFolds = sample(rep(unFoldsIn, length.out = sum(idTrain)))
                eIn =  lapply(unFoldsIn, function(inf){
                    idTrainIn = inFolds!=inf
                    modTrain = predFun(y, x, idTrainIn)
                    predTest = evalPredFun(modTrain, x[!idTrainIn])
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
            modTrain = predFun(y, x, seq_along(y)) #Fit on full model
            eOut = evalPredFun(modTrain, x[-id,]) #Out of sample prediction
            eIn = evalPredFun(modTrain, x) #In sample prediction
            ErrOutOfSample = mean((eOut-y[-id])^2) #Out of sample error
            ErrInSample = mean((eIn-y)^2)#In sample error
            #c("ErrOutOfSample" = ErrOutOfSample, "ErrInSample" = ErrInSample)
            #Inner bootstrap
            oobInner = lapply(integer(nInnerBootstraps), function(ii){
                idIn = sample(id, replace = TRUE)
                bootOob(y, x, idIn, id)
            })
            bootObjOOBests = processOob(oobInner)
            bootObj632ests =  process632In(bootObj632$obsBoot, oobObj = bootObjOOBests$procOb)
        })
    }
}
n = 40;p=3
y = rnorm(n)
x = matrix(rnorm(n*p),n,p)
predFun = function(y, x, id){lm.fit(y = y[id], x = cbind(1, x[id,]))}
evalPredFun = function(mod, x) {cbind(1,x) %*% mod$coef}
