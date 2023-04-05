#' Estimate correlation between MSE and MST estimators
#'
#' @inheritParams R2oosse
#'
#' @return the estimated correlation
#' @importFrom BiocParallel bplapply
estCorMSEMST = function(y, x, fitFun, predFun, methodMSE, methodCor, nBootstrapsCor, nFolds){
    nReps = switch(methodCor, "nonparametric" = nBootstrapsCor, "jackknife" = n <- length(y))
    matMSEMST = simplify2array(bplapply(seq_len(nReps), function(i){
            id = switch(methodCor, "nonparametric" = sample(length(y), replace = TRUE), "jackknife" = -i)
            c("MSEest" = switch(methodMSE,
                                "bootstrap" = boot632(y, x, id, fitFun, predFun),
                                "CV" = simpleCV(y, x, fitFun, predFun, nFolds)),
              "MSTest" = var(y[id]))
        }))
    corMSEMST = cor(matMSEMST[1,], matMSEMST[2,])
}