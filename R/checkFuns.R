#' Check whether supplied prediction function meets the requirements
#'
#' @param fitFun The prediction function, or its name as character string
#' @param reqArgs The vector of required arguments
#'
#' @return Throws an error when requirements not met, otherwise returns the function
checkFitFun = function(fitFun, reqArgs = c("y", "x")){
    fitFun = match.fun(fitFun)
    if(!all(id <- (reqArgs %in%  (args <- formalArgs(fitFun))))){
        stop("Fitting function does not accept\n", paste(reqArgs[!id], collapse = ", "), "\nas argument")
    } else {
        return(fitFun)
    }
}
checkPredFun = function(predFun, reqArgs = c("mod", "x")){
    predFun = match.fun(predFun)
    if(!all(id <- (reqArgs %in%  (args <- formalArgs(predFun))))){
        stop("Prediction function does not accept\n", paste(reqArgs[!id], collapse = ", "), "\nas argument")
    } else {
        return(predFun)
    }
}