#' Check whether supplied prediction function meets the requirements
#'
#' @param predFun The prediction function, or its name as character string
#' @param reqArgs The vector of required arguments
#'
#' @return Throws an error when requirements not met, otherwise returns the function
checkPredFun = function(predFun, reqArgs = c("y", "x", "id")){
    predFun = match.fun(predFun)
    if(!all(id <- (reqArgs %in%  (args <- formalArgs(predFun))))){
        stop("Arguments", paste(reqArgs[id], sep = ","), "not accepted by prediction function")
    } else {
        return(predFun)
    }
}