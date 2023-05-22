#' Calculate a confidence interval for R², MSE and MST
#'
#' @param oosseObj The result of the R2oosse call
#' @param what For which property should the ci be found: R² (default), MSE or MST
#' @param conf the confidence level required
#'
#' @return A vector of length 2 with lower and upper bound of the confidence interval
#' @details The upper bound of the interval is truncated at 1 for the R² and
#' the lower bound at 0 for the MSE
#' @importFrom stats qnorm
#' @export
#' @seealso \link{R2oosse}
#' @details The confidence intervals for R² and the MSE are based on standard errors and normal approximations.
#' The confidence interval for the MST is based on the chi-squared distribution as in equation (16) of \insertCite{Harding2014}{oosse},
#' but with inflation by a factor (n+1)/n. All quantities are out-of-sample.
#'
#' @examples
#' data(Brassica)
#' fitFunLM = function(y, x){lm.fit(y = y, x = cbind(1, x))}
#' predFunLM = function(mod, x) {cbind(1,x) %*% mod$coef}
#' R2lm = R2oosse(y = Brassica$Pheno$Leaf_8_width, x = Brassica$Expr[, 1:10],
#' fitFun = fitFunLM, predFun = predFunLM, nFolds = 5)
#' # A higher number of folds (e.g. 10) is recommended if computational resources allow
#' buildConfInt(R2lm)
#' buildConfInt(R2lm, what = "MSE")
#' buildConfInt(R2lm, what = "MST")
#' @references
#'    \insertAllCited{}
buildConfInt = function(oosseObj, what = c("R2", "MSE", "MST"), conf = 0.95){
    stopifnot(conf >0, conf <1)
    what = match.arg(what)
    bounds <- c((1-conf)/2, conf + (1-conf)/2)
    if(what %in% c("R2", "MSE")){
        zQuants = qnorm(bounds)
        obj = oosseObj[[what]]
        ci = with(oosseObj, obj[what] + obj[paste0(what,"SE")]*zQuants)
        if(what == "R2"){
            ci[2] = min(ci[2], 1)#Truncate at 1
        } else if(what == "MSE"){
            ci[1] = max(ci[1], 0)#Truncate at 0
        }
    } else if (what == "MST"){
        obj = oosseObj[[what]]
        ci = with(oosseObj, obj[what]*((n-1)/qchisq(p = bounds[2:1], df = (n-1))))
    }
    names(ci) = paste0(bounds*100, "%")
    return(ci)
}