estMSEandSE = function(y, x, predFun, methodMSE = c("CV", "bootstrap"), nFolds = 10, cvReps = 200, nBootstraps = 200, ...){
    predFun = match.fun(predFun)
    methodMSE = match.arg(methodMSE)
    if((n <- length(y)) != NROW(x)){
        stop("Number of observations in y and x must match!")
    }
    #Predict time this will take
    singleRunTime = system.time(fullModel <- predFun(y, x, ...))
    cat("Fitting the model once took", formatSeconds(singleRunTime), ".\n", "You requested",
        switch(methodMSE,
               "CV" = paste0(cvReps, " repeats of ", nFolds, "-fold cross-validation"),
               "bootstrap" = paste(nBootstraps, ".632 bootstrap instances")),
    "with", nCores <- multicoreWorkers(), "cores, which is expected to last for\n",
    formatSeconds(singleRunTime/nCores * switch(methodMSE, "CV" = cvReps*nFolds, "bootstraps" = nBootstraps)))

    cvSplitReps = lapply(seq_len(cvReps), function(cvs){
        folds = sample(rep(unFolds <- seq_len(nFolds), length.out = n))
        lapply(unFolds, function(uf){
            idTrain = folds!=uf
            yTrain = dat$y[idTrain]; xTrain <- dat$x[idTrain,j];locTrain = loc[idTrain,]
            predTest = predLin(xTrain, yTrain, dat$x[!idTrain, j], loc = loc[idTrain,], ...)
            eOut = (predTest-dat$y[!idTrain])^2
            eBarOut = mean(eOut)
            b = var(eOut)/length(eOut)
            inFolds = sample(rep(unFoldsIn <- seq_len(nInnerFolds), length.out = sum(idTrain)))
            errHatTilde = mean(vapply(FUN.VALUE = double(1), unFoldsIn, function(inf){
                idTrainIn = inFolds!=inf
                predTestIn = predLin(xTrain[idTrainIn], yTrain[idTrainIn], xTrain[!idTrainIn], loc = locTrain[idTrainIn,],...)
                mean((predTestIn-yTrain[!idTrainIn])^2)
            }))
            a = (errHatTilde-eBarOut)^2
            list("a" = a, "b" = b, "errHatTilde" = errHatTilde, "eOut" = eOut, "margVar" = var(dat$y[!idTrain]))
        })
    })
}
