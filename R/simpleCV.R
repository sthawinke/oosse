#' Perform simple CV, and return the MSE estimate
#'
#' @inheritParams oosse
#' @inheritParams estLoss
#' @inheritParams estModelLoss
#' @return A vector of length 2: The MSE estimate, and mean prediction
simpleCV <- function(y, x, fitFun, predFun, nFolds, loss, skillScore, yMat) {
    folds <- sample(rep(unFolds <- seq_len(nFolds), length.out = length(y)))
    simList <- lapply(unFolds, function(uf) {
        idTrain <- folds != uf
        predTest <- predFun(
            fitFun(y[idTrain], x[idTrain, , drop = FALSE]),
            x[!idTrain, , drop = FALSE]
        )
        loss <- estLoss(subsetY(y, yMat, skillScore, !idTrain), predTest, loss = loss)
        list("loss" = loss, "predTest" = predTest)
    })
    meanLoss <- mean(unlist(lapply(simList, function(x) x[["loss"]])))
    kappaHat <- mean(unlist(lapply(simList, function(x) x[["predTest"]])))
    c("loss" = meanLoss, "kappaHat" = kappaHat)
}
