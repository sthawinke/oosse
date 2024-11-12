#' The .632 bootstrap estimation of the MSE
#'
#' @inheritParams oosse
#' @inheritParams estLoss
#' @inheritParams estModelLoss
#' @param id the sample indices resampled with replacement
#'
#' @details The implementation follows \insertCite{Efron1997}{oosse}
#' @return A vector of length 2: the MSE estimate, and the average prediction
#' @seealso \link{estModelLoss} \link{bootOob}
#' @references
#'   \insertAllCited{}
boot632 = function(y, x, id, fitFun, predFun, loss, yMat, skillScore){
            modTrain = fitFun(y, x) #Fit on full model
            eOut = predFun(modTrain, x[-id, , drop = FALSE]) #Out of sample prediction
            eIn = predFun(modTrain, x) #In sample prediction
            ErrOutOfSample = mean(estLoss(subsetY(y, yMat, skillScore, -id), eOut, loss)) #Out of sample error
            ErrInSample = mean(estLoss(if(skillScore == "RankedProbability") yMat else y, eIn, loss))#In sample error
            expvec = c(exp(-1), 1-exp(-1))
            c("loss" = sum(expvec*c(ErrInSample, ErrOutOfSample)), "kappaHat" = mean(eOut))
}
#' Repeated .632 bootstrapa
#'
#' @inheritParams oosse
#' @param ... passed onto boot632
#'
#' @return The estimated MSE
boot632multiple = function(nBootstraps, y, ...){
    tmp = vapply(seq_len(nBootstraps), FUN.VALUE = double(2), function(br){
        id = sample(length(y), replace = TRUE)
        boot632(y = y, id = id, ...)
    })
    return(rowMeans(tmp))
}
