#' Estimate MSE and its standard error
#'
#' @inheritParams oosse
#' @inheritParams estLoss
#' @return A vector with MSE estimate and its standard error
#' @importFrom parallel parLapply
#' @importFrom stats var
#' @details The nested cross-validation scheme follows \insertCite{Bates2023}{oosse},
#' the .632 bootstrap is implemented as in \insertCite{Efron1997}{oosse}
#' @references
#'   \insertAllCited{}
estModelLoss = function(y, x, fitFun, predFun, methodLoss, nFolds, nInnerFolds,
                  cvReps, nBootstraps, loss){
        n <- length(y)
        seVec = if(methodLoss == "CV"){
                #Nested cross-validation
                unFolds <- seq_len(nFolds);unFoldsIn <- seq_len(nInnerFolds) #Prepare the folds
                cvSplitReps = bplapply(seq_len(cvReps), function(cvs){
                    folds = sample(rep(unFolds, length.out = n))
                    lapply(unFolds, function(uf){
                        #Outer loop
                        idTrain = folds!=uf
                        modTrain = fitFun(y[idTrain], x[idTrain,,drop = FALSE])
                        predTest = predFun(modTrain, x[!idTrain, , drop = FALSE])
                        eOut = estLoss(y[!idTrain], predTest, loss = loss)
                        #Inner loop
                        inFolds = sample(rep(unFoldsIn, length.out = sum(idTrain)))
                        eIn = lapply(unFoldsIn, function(inf){
                            idTrainIn = inFolds!=inf
                            modTrain = fitFun(y[idTrainIn], x[idTrainIn,,drop = FALSE])
                            predTest = predFun(modTrain, x[!idTrainIn, , drop = FALSE])
                            estLoss(y[!idTrainIn], predTest, loss = loss)
                        })
                        #summary statistics
                        errHatTilde = mean(unlist(eIn), na.rm = TRUE)
                        a = (errHatTilde-mean(eOut, na.rm = TRUE))^2
                        b = var(eOut, na.rm = TRUE)/length(eOut)
                        list("a" = a, "b" = b, "errHatTilde" = errHatTilde, "eOut" = eOut)
                    })
                })
                getSEsNested(cvSplitReps, nFolds, n = n)
            } else if(methodLoss == "bootstrap") {
                    bootReps = bplapply(seq_len(nBootstraps), function(br){
                        id = sample(n, replace = TRUE)
                        #.632 bootstrap
                        MSE632est = boot632(y, x, id, fitFun, predFun, loss = loss)
                        #Out of bag bootstrap
                        oob = bootOob(y, x, id, fitFun, predFun, loss = loss)
                        list("oobObj" = oob, "MSE632est" = MSE632est)
                    })
                    MSE632est = mean(vapply(FUN.VALUE = double(1), bootReps, function(x) {x$MSE632est}), na.rm = TRUE)
                    MSEoob = processOob(bootReps)
                    SEmse = unname(MSE632est/MSEoob["MSEhat"]*MSEoob["SEhat"])
                    c("Estimate" = MSE632est, "StandardError" = SEmse)
            }
    return(seVec)
}
