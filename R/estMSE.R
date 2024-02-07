#' Estimate MSE and its standard error
#'
#' @inheritParams R2oosse
#' @return A vector with MSE estimate and its standard error
#' @importFrom parallel parLapply
#' @importFrom stats var
#' @details The nested cross-validation scheme follows \insertCite{Bates2023}{oosse},
#' the .632 bootstrap is implemented as in \insertCite{Efron1997}{oosse}
#' @references
#'   \insertAllCited{}
estMSE = function(y, x, fitFun, predFun, methodMSE, nFolds, nInnerFolds,
                  cvReps, nBootstraps){
        n <- length(y)
        seVec =  if(methodMSE == "CV"){
            #Nested cross-validation
            unFolds <- seq_len(nFolds);unFoldsIn <- seq_len(nInnerFolds) #Prepare the folds
            cvSplitReps = bplapply(seq_len(cvReps), function(cvs){
                folds = sample(rep(unFolds, length.out = n))
                lapply(unFolds, function(uf){
                    #Outer loop
                    idTrain = folds!=uf
                    modTrain = fitFun(y[idTrain], x[idTrain,,drop = FALSE])
                    predTest = predFun(modTrain, x[!idTrain, , drop = FALSE])
                    eOut = (predTest-y[!idTrain])^2
                    #Inner loop
                    inFolds = sample(rep(unFoldsIn, length.out = sum(idTrain)))
                    eIn =  lapply(unFoldsIn, function(inf){
                        idTrainIn = inFolds!=inf
                        modTrain = fitFun(y[idTrainIn], x[idTrainIn,,drop = FALSE])
                        predTest = predFun(modTrain, x[!idTrainIn, , drop = FALSE])
                        (predTest-y[!idTrainIn])^2
                    })
                    #summary statistics
                    errHatTilde = mean(unlist(eIn))
                    a = (errHatTilde-mean(eOut))^2
                    b = var(eOut)/length(eOut)
                    list("a" = a, "b" = b, "errHatTilde" = errHatTilde, "eOut" = eOut)
                })
            })
            getSEsNested(cvSplitReps, nFolds, n = n)
    } else if(methodMSE == "bootstrap") {
            bootReps = bplapply(seq_len(nBootstraps), function(br){
                id = sample(n, replace = TRUE)
                #.632 bootstrap
                MSE632est = boot632(y, x, id, fitFun, predFun)
                #Out of bag bootstrap
                oob = bootOob(y, x, id, fitFun, predFun)
                list("oobObj" = oob, "MSE632est" = MSE632est)
            })
            MSE632est = mean(vapply(FUN.VALUE = double(1), bootReps, function(x) {x$MSE632est}))
            MSEoob = processOob(bootReps)
            SEmse = unname(MSE632est/MSEoob["MSEhat"]*MSEoob["SEhat"])
            c("MSE" = MSE632est, "MSESE" = SEmse)
    }
return(seVec)
}