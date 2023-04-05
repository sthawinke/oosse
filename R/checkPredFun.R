#' Check whether supplied prediction function meets the requirements
#'
#' @param fitFun The prediction function, or its name as character string
#' @param reqArgs The vector of required arguments
#'
#' @return Throws an error when requirements not met, otherwise returns the function
checkFitFun = function(fitFun, reqArgs = c("y", "x", "id")){
    fitFun = match.fun(fitFun)
    if(!all(id <- (reqArgs %in%  (args <- formalArgs(fitFun))))){
        stop("Arguments", paste(reqArgs[id], sep = ","), "not accepted by prediction function")
    } else {
        return(fitFun)
    }
}