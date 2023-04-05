#' The .632 bootstrap estimation of the MSE
#'
#' @inheritParams estMSEandSE
#'
#' @return The MSE estimate
boot632 = function(y, x, id, fitFun, predFun){
            modTrain = fitFun(y, x, seq_along(y)) #Fit on full model
            eOut = predFun(modTrain, x[-id,]) #Out of sample prediction
            eIn = predFun(modTrain, x) #In sample prediction
            ErrOutOfSample = mean((eOut-y[-id])^2) #Out of sample error
            ErrInSample = mean((eIn-y)^2)#In sample error
            expvec = c(exp(-1), 1-exp(-1))
            sum(expvec*c(ErrInSample, ErrOutOfSample))
        }
