#The oob bootstrap (smooths leave-one-out CV)
bootOob = function(y, x, id, id0){
    id2 = id0[-id]
    Nis = vapply(id0, FUN.VALUE = integer(1), function(x) sum(x==id))
    Eis = {
        predTest = evalPredFun(predFun(x = x[id,], y = y[id]), x[id2,])
        (predTest-y[id2])^2
    }
    cbind(Eis, "Nis" = Nis)
}
processOob = function(oobObj){
    procOb = processOobIn(oobObj$obsBoot) #Process observed and bootstrap
    procBoot = lapply(oobObj$bootBoot, processOobIn)
    list('procOb' = procOb, 'procBoot' = procBoot)
}
processOobIn = function(x){
    Nmat = sapply(x, function(y) y[, "Nis"])
    n = nrow(Nmat)
    Imat = Nmat==0
    rI = rowSums(Imat)
    IQmat = vapply(FUN.VALUE = Nmat, seq_len(ncol(x[[1]])-1), function(j){
        vapply(FUN.VALUE = double(n), x, function(y){y[,j]})*Imat
    })
    Eis = apply(IQmat, c(1,3), sum)/rI
    errEsts = colMeans(Eis)
    # Following Efron1997, equation (40)
    qMat = colMeans(IQmat)
    Dis = (2+1/(n-1))*(Eis-errEsts)/n + ((Nmat-rowMeans(Nmat)) %*% qMat)/rI
    seEsts = sqrt(colSums(Dis^2))
    cbind("MSEhat" = errEsts, "SEhat" = seEsts, "SEhatNaive" = apply(Eis, 2, sd)/sqrt(n))
}
process632In = function(x, oobObj = NULL){
    bootEsts = matrix(sapply(x, function(y) sum(y$bootRes*expvec)), ncol = length(x))
    MSEhat = rowMeans(bootEsts)
    if(!is.null(oobObj)){
        SEhatNaive = sd(bootEsts)
        SEhat = MSEhat/oobObj[, "MSEhat"]*oobObj[, "SEhat"]
    } else {
        SEhat = SEhatNaive = NULL
    }
    rbind("MSEhat" = MSEhat, "SEhat" = SEhat, "SEhatNaive" = SEhatNaive)
}
