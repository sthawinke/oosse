#' The .632 bootstrap estimation of the MSE
#'
#' @inheritParams oosse
#' @inheritParams estLoss
#' @inheritParams estModelLoss
#' @param id the sample indices resampled with replacement
#'
#' @details The implementation follows \insertCite{Efron1997}{oosse}
#' @return The MSE estimate
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
            sum(expvec*c(ErrInSample, ErrOutOfSample))
}
#' Repeated .632 bootstrapa
#'
#' @inheritParams oosse
#' @param ... passed onto boot632
#'
#' @return The estimated MSE
boot632multiple = function(nBootstraps, y, ...){
    mean(unlist(lapply(seq_len(nBootstraps), function(br){
        id = sample(length(y), replace = TRUE)
        boot632(y = y, id = id, ...)
    })))
}
