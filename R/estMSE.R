#' Estimate MSE and its standard error
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