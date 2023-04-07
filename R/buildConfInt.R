#' Calculate a confidence interval for the R²
#'
#' @param oosseObj The result of the R2oosse call
#' @param what For which property should the ci be found: R² (default), MSE or MST
#' @param conf the confidence level required
#'
#' @return A vector of length2 with lower and upper bound of the confidence interval
#' @details The upper bound of the interval is truncated at 1 for the R² and
#' the lower bound at 0 for the MSE
#' @importFrom stats qnorm
#' @export
#' @seealso \link{R2oosse}
#'
#' @examples
#' data(Brassica)
#' fitFunLM = function(y, x){lm.fit(y = y, x = cbind(1, x))}
#' predFunLM = function(mod, x) {cbind(1,x) %*% mod$coef}
#' R2lm = R2oosse(y = Brassica$Pheno$Leaf_8_width, x = Brassica$Expr[, 1:10],
#' fitFun = fitFunLM, predFun = predFunLM, nFolds = 5)
#' # A higher number of folds (e.g. 10) is recommended if computational resources allow
#' buildConfInt(R2lm)
buildConfInt = function(oosseObj, what = c("R2", "MSE"), conf = 0.95){
    what = match.arg(what)
    zQuants = qnorm(bounds <- c((1-conf)/2, conf + (1-conf)/2))
    obj = oosseObj[[what]]
    ci = with(oosseObj, obj[what] + obj[paste0(what,"SE")]*zQuants)
    if(what == "R2"){
        ci[2] = min(ci[2], 1)#Truncate at 1
    } else if(what == "MSE"){
        ci[1] = max(ci[1], 0)#Truncate at 0
    }
    names(ci) = paste0(bounds*100, "%")
    return(ci)
}