#' Perform simple CV, and return the MSE estimate
#'
#' @inheritParams estMSEandSE
#' @return The MSE estimate
simpleCV = function(y, x, predFun, evalPredFun, nFolds){
    folds = sample(rep(unFolds <- seq_len(nFolds), length.out = length(y)))
    mean(unlist(lapply(unFolds, function(uf){
            idTrain = folds!=uf
            predTest = evalPredFun(predFun(x[idTrain,], y[idTrain]), x[!idTrain,])
            (predTest-y[!idTrain])^2
    })))
}
