#' Perform simple CV, and return the MSE estimate
#'
#' @inheritParams R2oosse
#' @return The MSE estimate
simpleCV = function(y, x, fitFun, predFun, nFolds, cvReps, ...){
    unFolds <- seq_len(nFolds)
    mean(unlist(vapply(seq_len(cvReps), FUN.VALUE = y, function(ii){
        folds = sample(rep(unFolds, length.out = length(y)))
        unlist(lapply(unFolds, function(uf){
            idTrain = folds!=uf
            predTest = predFun(fitFun(y[idTrain], x[idTrain,,drop = FALSE], ...),
                               x[!idTrain,,drop = FALSE], ...)
            (predTest-y[!idTrain])^2
        }))
    })))
}
