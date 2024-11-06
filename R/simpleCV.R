#' Perform simple CV, and return the MSE estimate
#'
#' @inheritParams oosse
#' @inheritParams estLoss
#' @inheritParams estModelLoss
#' @return The MSE estimate
simpleCV = function(y, x, fitFun, predFun, nFolds, loss, skillScore, yMat){
    folds = sample(rep(unFolds <- seq_len(nFolds), length.out = length(y)))
    mean(unlist(lapply(unFolds, function(uf){
            idTrain = folds!=uf
            predTest = predFun(fitFun(y[idTrain], x[idTrain,,drop = FALSE]),
                               x[!idTrain,,drop = FALSE])
            estLoss(subsetY(y, yMat, skillScore, !idTrain), predTest, loss = loss)
    })))
}
