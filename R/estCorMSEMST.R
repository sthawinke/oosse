#' Estimate correlation between MSE and MST estimators
#'
#' @inheritParams R2oosse
#'
#' @return the estimated correlation
#' @importFrom parallel parLapply
estCorMSEMST = function(y, x, fitFun, predFun, methodMSE, methodCor, nBootstrapsCor, nFolds, nBootstraps){
    nReps = switch(methodCor, "nonparametric" = nBootstrapsCor, "jackknife" = length(y))
    matMSEMST = simplify2array(bplapply(seq_len(nReps), function(i){
            id = switch(methodCor, "nonparametric" = sample(length(y), replace = TRUE), "jackknife" = -i)
            c("MSEest" = switch(methodMSE,
                                "bootstrap" = boot632multiple(nBootstraps = nBootstraps, y[id], x[id,,drop = FALSE], fitFun, predFun),
                                "CV" = simpleCV(y[id], x[id, ,drop = FALSE], fitFun, predFun, nFolds)),
              "MSTest" = var(y[id]))
        }))
    corMSEMST = cor(matMSEMST[1,], matMSEMST[2,])
    return(corMSEMST)
}