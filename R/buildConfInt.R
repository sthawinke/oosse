#' Calculate a confidence interval for the R²
#'
#' @param oosseObj The result of the R2oosse call
#' @param what FOr which property should the ci be found: R² (default), MSE or MST
#' @param conf the confidence level required
#'
#' @return
#' @importFrom stats qnorm
#' @export
#'
#' @examples
#' data(Brassica)
#' fitFunLM = function(y, x){lm.fit(y = y, x = cbind(1, x))}
#' predFunLM = function(mod, x) {cbind(1,x) %*% mod$coef}
#' R2lm = R2oosse(y = Brassica$Pheno$Leaf_8_width, x = Brassica$Expr[, 1:10],
#' fitFun = fitFunLM, predFun = predFunLM)
#' buildConfInt(R2lm)
buildConfInt = function(oosseObj, what = c("R2", "MSE", "MST"), conf = 0.95){
    what = match.arg(what)
    zQuants = qnorm(bounds <- c((1-conf)/2, conf + (1-conf)/2))
    ci = with(oosseObj, R2[what] + R2[paste0(what,"SE")]*zQuants)
    names(ci) = paste0(bounds*100, "%")
    return(ci)
}