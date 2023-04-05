#The oob bootstrap (smooths leave-one-out CV)
bootOob = function(y, x, id, id0, fitFun, predFun){
    id2 = id0[-id]
    Eis = double(length(id))
    Nis = vapply(id0, FUN.VALUE = integer(1), function(x) sum(x==id))
    Eis[id2] = {
        predTest = predFun(fitFun(x = x[id,], y = y[id]), x[id2,])
        (predTest-y[id2])^2
    }
    cbind(Eis, "Nis" = Nis)
}
processOob = function(x){
    Nmat = sapply(x, function(y) y$oobObj[, "Nis"])
    n = nrow(Nmat)
    Imat = Nmat==0
    rI = rowSums(Imat)
    IQmat = vapply(FUN.VALUE = double(n), x, function(y){y$oobObj[,"Eis"]})*Imat
    Eis = rowSums(IQmat)/rI
    errEst = sum(Eis)
    # Following Efron1997, equation (40)
    qMat = colMeans(IQmat)
    Dis = (2+1/(n-1))*(Eis-errEst)/n + ((Nmat-rowMeans(Nmat)) %*% qMat)/rI
    seEst = sqrt(sum(Dis^2))
    c("MSEhat" = errEst, "SEhat" = seEst)#, "SEhatNaive" = sd(Eis)/sqrt(n))
}
