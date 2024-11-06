#' Estimate correlation between MSE and MST estimators
#'
#' @inheritParams oosse
#' @inheritParams estLoss
#'
#' @return the estimated correlation
#' @importFrom BiocParallel bplapply
#' @importFrom stats cor
estCorMeanRef = function(y, x, fitFun, predFun, methodLoss, methodCor, nBootstrapsCor, nFolds, nBootstraps, loss, skillScore, yMat){
    nReps = switch(methodCor, "nonparametric" = nBootstrapsCor, "jackknife" = length(y))
    matMSEMST = simplify2array(bplapply(seq_len(nReps), function(i){
            id = switch(methodCor, "nonparametric" = sample(length(y), replace = TRUE), "jackknife" = -i)
            y = subsetY(y, yMat, skillScore, id)
            c("modelLoss" = switch(methodLoss,
                                "bootstrap" = boot632multiple(nBootstraps = nBootstraps, y, x[id,,drop = FALSE], fitFun, predFun, loss = loss),
                                "CV" = simpleCV(y, x[id, ,drop = FALSE], fitFun, predFun, nFolds, loss = loss)),
              "referenceLoss" = estRefLoss(y, x[id, ,drop = FALSE], skillScore)["Estimate"])
        }))
    corMSEMST = cor(matMSEMST[1,], matMSEMST[2,], use = "complete.obs")
    return(corMSEMST)
}