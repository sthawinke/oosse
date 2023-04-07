#' Calculate standard error on MSE from nested CV results
#'
#' @param cvSplitReps The list of outer and inner CV results
#' @param nOuterFolds The number of outer folds
#' @param n The sample size
#'
#' @return The estimate of the MSE and its standard error
#' @details The calculation of the standard error of the MSE as proposed by \insertCite{Bates2021}{oosse}
#' @seealso \link{estMSE}
#' @references
#'   \insertAllCited{}
getSEsNested = function(cvSplitReps, nOuterFolds, n){
    ErrNCV = mean(vapply(cvSplitReps, FUN.VALUE = double(nOuterFolds),
                         function(y) vapply(y, FUN.VALUE = double(1), function(x) x[["errHatTilde"]])))
    MSEhat = mean(vapply(cvSplitReps, FUN.VALUE = double(nOuterFolds), function(y) vapply(y, FUN.VALUE = double(1),function(x) x[["a"]]-x[["b"]])))
    errOuter0 = lapply(cvSplitReps, function(y) lapply(y, function(x) x[["eOut"]]))
    mseOuter = vapply(FUN.VALUE = double(nOuterFolds), errOuter0, function(w) vapply(FUN.VALUE = double(1), w, mean))
    errOuter = unlist(errOuter0)
    SEest = sqrt(max(0, nOuterFolds/(nOuterFolds-1)*MSEhat))
    naiveRMSE = sd(errOuter)/sqrt(n)
    maxMSE = naiveRMSE * sqrt(nOuterFolds)
    if(is.na(SEest) || (SEest < naiveRMSE)){ #See below equation (17), prevent implausible values
        SEest = naiveRMSE
    } else if (SEest > maxMSE){
        SEest = maxMSE
    }
    #Correct the bias
    ErrCV = mean(errOuter)
    Bias = (1+(nOuterFolds-2)/nOuterFolds)*(ErrNCV-ErrCV)
    ErrNCVBC = ErrNCV - Bias#Bias correction
    c("MSE" = ErrNCVBC, "MSESE" = SEest)
}