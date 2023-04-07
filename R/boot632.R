#' The .632 bootstrap estimation of the MSE
#'
#' @inheritParams R2oosse
#' @param id the sample indices resampled with replacement
#'
#'@details The implementation follows \cite{Efron1997}
#' @return The MSE estimate
boot632 = function(y, x, id, fitFun, predFun){
            modTrain = fitFun(y, x) #Fit on full model
            eOut = predFun(modTrain, x[-id, , drop = FALSE]) #Out of sample prediction
            eIn = predFun(modTrain, x) #In sample prediction
            ErrOutOfSample = mean((eOut-y[-id])^2) #Out of sample error
            ErrInSample = mean((eIn-y)^2)#In sample error
            expvec = c(exp(-1), 1-exp(-1))
            sum(expvec*c(ErrInSample, ErrOutOfSample))
}
boot632multiple = function(nBootstraps, y, ...){
    mean(unlist(lapply(seq_len(nBootstraps), function(br){
        id = sample(length(y), replace = TRUE)
        boot632(y = y, id = id, ...)
    })))
}
