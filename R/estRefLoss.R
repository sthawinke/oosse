#' Estimate the out-of sample loss for the reference model
#'
#'The reference model taken here is the mean of the training data
#'
#' @return A vector of length 2: the estimated reference loss and its standard error.
#' For the Peirce skill score, a vector of length 4: the reference loss of two different reference models, and their standard error
#' @inheritParams oosse
#' @importFrom stats pbinom
estRefLoss = function(y, x, skillScore){
    n = length(y)
    yBar = switch(skillScore,
                  "RankedProbability" = table(y)/n,
                  mean(y))
    margVar = if(skillScore %in% c("R2", "Brier")){
        var(y)
    }
    out = if(skillScore == "R2"){
        MST <- margVar*(n+1)/n
        c(MST, sqrt(2/(n-1))*MST)
    } else if(skillScore == "Brier"){
        c(margVar*(n+1)/n, sqrt((1-2*yBar)^2*yBar*(1-yBar))*(n+1)/(n-1)^{3/2})
    } else if(skillScore == "Peirce"){
        lrAnaObs = 2*yBar*(1-yBar)*(n)/(n-1)
        deltaSEObs = sqrt(4*(1-2*yBar)^2*yBar*(1-yBar)*n^2/(n-1)^3)
        lrAnaPred = lrAnaMod
        lrAnaMod = function(yBar, kappaHat, n){
            yBar*(1-kappaHat) + (1-yBar)*kappaHat + 2 covEst
        }
        lrAnaObs = function(yBar, n){
            lrAna = 2*yBar*(1-yBar)*(n)/(n-1)
            deltaSE = sqrt(4*(1-2*yBar)^2*yBar*(1-yBar)*n^2/(n-1)^3)
            c(lrAna, deltaSE)
        }
        deltaSEpred
        c(lrAna, deltaSE)
    } else if(skillScore == "Heidke"){
        lrAna =  lrAnaMod()
        deltaSE = sqrt(4*(1-2*yBar)^2*yBar*(1-yBar)*n^2/(n-1)^3)
        c(lrAna, deltaSE)
    } else if(skillScore == "AgnosticHeidkeHeidke"){
        lrAnaObs(yBar, n)
    } else if(skillScore == "Appleman"){
        lrAna = yBar*pbinom(n/2, size = n, prob = yBar) +
            (1-yBar)*pbinom(n/2, size = n, prob = yBar, lower.tail = FALSE)
        lrAnaBC = lrAna - 2*estCov(n , yBar) #Bias correction
        deltaSE = abs(1+prFunDerivFull(yBar,n))*sqrt((yBar*(1-yBar)/(n-1)))
        c(lrAnaBC, deltaSE)
    } else if(skillScore == "McFadden"){
        lrAna = -(yBar*log(yBar) + (1-yBar)*log(1-yBar))
        lrAnaBC = lrAna + estCov(n, yBar, "logLoss") + estCov(n, yBar, "logLossMin") #Bias correction
        deltaSE = abs(log(yBar/(1-yBar))*sqrt(yBar*(1-yBar)/(n-1)))
        c(lrAnaBC, deltaSE)
    } else if(skillScore == "RankedProbability"){
        lrAna = sum(yBar*(1-yBar))*(n+1)/n
        vcovar = buildVarCovarMult(yBar, n)
        gradient = ((n+1)/(n-1))^2*(1-2*yBar)^2*yBar*(1-yBar)
        deltaSE = sqrt(crossprod(gradient, vcovar) %*% gradient)
        c(lrAna, deltaSE)
    }
    names(out) = c("Estimate", "StandardError")
    return(out)
}
