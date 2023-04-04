checkPredFun = function(predFun, reqArgs = c("y", "x", "id")){
    predFun = match.fun(predFun)
    if(!all(id <- (reqArgs %in%  (args <- formalArgs(predFun))))){
        stop("Arguments", pqste(reqArgs[id], sep = ","), "not accepted by prediction function")
    }
}