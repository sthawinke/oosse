#' Estimate the out-of sample loss for the reference model
#'
#'The reference model taken here is the mean of the training data
#'
#' @param margVar The observed marginal variance
#'
#' @return A vector of length 2: the estimated reference loss and its standard error
#' @inheritParams oosse
#' @importFrom stats pbinom
estRefLoss = function(y, x, margVar, skillScore, estCovMethod, nBootstraps){
    n = length(y)
    yBar = mean(y)
    MST = margVar*(n+1)/n
    out = if(skillScore == "R2"){
        c(MST, sqrt(2/(n-1))*MST)
    } else if(skillScore == "Brier"){
        c(MST, sqrt((1-2*yBar)^2*yBar*(1-yBar))*(n+1)/(n-1)^{3/2})
    } else if(skillScore == "Heidke"){
        lrAna = 2*yBar*(1-yBar)*(n)/(n-1)
        varEst = 4*(1-2*yBar)^2*yBar*(1-yBar)*n^2/(n-1)^3
        c(lrAnaBC, sqrt(varEst))
    } else if(skillScore == "Missclassification"){
        lrAna = yBar*pbinom(n/2, size = n, prob = yBar) +
            (1-yBar)*pbinom(n/2, size = n, prob = yBar, lower.tail = FALSE)
        lrAnaBC = lrAna - 2*estCov(y, estCovMethod = estCovMethod, nBootstraps) #Bias correction
        deltaSE = abs(1+prFunDerivFull(yBar,n))*sqrt((yBar*(1-yBar)/(n-1)))
        c(lrAnaBC, deltaSE)
    } else if(skillScore == "McFadden"){
        lrAna = -(yBar*log(yBar) + (1-yBar)*log(1-yBar))
        lrAnaBC = lrAna + estCovAna(n, yBar, "logLoss") + estCovAna(n, yBar, "logLossMin") #Bias correction
        deltaSE = abs(log(yBar/(1-yBar))*sqrt(yBar*(1-yBar)/(n-1)))
        c(lrAnaBC, deltaSE)
    }
    names(out) = c("Estimate", "StandardError")
    return(out)
}