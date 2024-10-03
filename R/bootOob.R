#' The oob bootstrap (smooths leave-one-out CV)
#'
#' @inheritParams oosse
#' @inheritParams estLoss
#' @param id sample indices sampled with replacement
#' @details The implementation follows \insertCite{Efron1997}{oosse}
#' @return matrix of errors and inclusion times
#' @seealso \link{estModelLoss} \link{boot632}
#' @references
#'   \insertAllCited{}
bootOob = function(y, x, id, fitFun, predFun, loss){
    id2 = (id0 <- seq_along(y))[-id]
    Eis = double(length(id))
    Nis = vapply(id0, FUN.VALUE = integer(1), function(x) sum(x==id))
    Eis[id2] = {
        predTest = predFun(fitFun(x = x[id,,drop = FALSE], y = y[id]), x[id2,,drop = FALSE])
        estLoss(y[id2], predTest, loss)
    }
    cbind(Eis, "Nis" = Nis)
}
#' Process the out-of-bag bootstraps to get to standard errors following Efron 1997
#'
#' @param x the list with out=of=bag bootstrap results
#'
#' @return out-of-bag MSE estimate and standard error
processOob = function(x){
    Nmat = sapply(x, function(y) y$oobObj[, "Nis"])
    n = nrow(Nmat)
    Imat = Nmat==0
    rI = rowSums(Imat, na.rm = TRUE)
    IQmat = vapply(FUN.VALUE = double(n), x, function(y){y$oobObj[,"Eis"]})*Imat
    Eis = rowSums(IQmat, na.rm = TRUE)/rI
    errEst = sum(Eis, na.rm = TRUE)
    # Following Efron1997, equation (40)
    qMat = colMeans(IQmat, na.rm = TRUE)
    Dis = (2+1/(n-1))*(Eis-errEst)/n + ((Nmat-rowMeans(Nmat, na.rm = TRUE)) %*% qMat)/rI
    seEst = sqrt(sum(Dis^2, na.rm = TRUE))
    c("MSEhat" = errEst, "SEhat" = seEst)
}
