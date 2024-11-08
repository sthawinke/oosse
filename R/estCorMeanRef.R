#' Estimate correlation between MSE and MST estimators
#'
#' @inheritParams oosse
#' @inheritParams estLoss
#' @inheritParams estModelLoss
#'
#' @return the estimated correlation
#' @importFrom BiocParallel bplapply
#' @importFrom stats cor
estCorMeanRef = function(y, x, fitFun, predFun, methodLoss, methodCor, nBootstrapsCor,
                         nFolds, nBootstraps, loss, skillScore, yMat){
    nReps = switch(methodCor, "nonparametric" = nBootstrapsCor, "jackknife" = length(y))
    matMSEMST = simplify2array(bplapply(seq_len(nReps), function(i){
            id = switch(methodCor, "nonparametric" = sample(length(y), replace = TRUE), "jackknife" = -i)
            c("modelLoss" = switch(methodLoss,
                                "bootstrap" = boot632multiple(nBootstraps = nBootstraps, y[id], x[id,,drop = FALSE], yMat = yMat[id,,drop = FALSE],
                                                              fitFun, predFun, loss = loss, skillScore = skillScore),
                                "CV" = simpleCV(y[id], x[id, ,drop = FALSE], yMat = yMat[id,,drop = FALSE],
                                                fitFun, predFun, nFolds, loss = loss, skillScore = skillScore)),
              "referenceLoss" = estRefLoss(y[id], x[id, ,drop = FALSE], skillScore)["Estimate"])
        }))
    corMSEMST = cor(matMSEMST[1,], matMSEMST[2,], use = "complete.obs")
    return(corMSEMST)
    #Add: 3x3 covariance matrix needed for Peirce
}