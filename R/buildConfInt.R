#' Calculate a confidence interval for R², MSE and MST
#'
#' @param oosseObj The result of the oosse call
#' @param what For which property should the ci be found. See details for options
#' @param conf the confidence level required
#'
#' @return A vector of length 2 with lower and upper bound of the confidence interval
#' @details The upper bound of the interval is truncated at 1 for the R² and
#' the lower bound at 0 for the MSE
#' @importFrom stats qnorm
#' @export
#' @seealso \link{oosse}
#' @details The confidence intervals for R² and the MSE are based on standard errors and normal approximations.
#' The confidence interval for the MST is based on the chi-squared distribution as in equation (16) of \insertCite{Harding2014}{oosse},
#' but with inflation by a factor (n+1)/n. All quantities are out-of-sample.
#' Possible options for "what" are 'R2', 'MSE', 'MST', 'BrierScore', 'BrierSkillScore', 'ReferenceBrierScore',
#' 'ModelMisclassRate', 'ReferenceMisclassRate' and 'PeirceSkillScore'. For all but the MST,
#' normal distributions are used to construct the confidence intervals; for the MST the chi-squared distribution is used.
#'
#' @examples
#' data(Brassica)
#' fitFunLM = function(y, x){lm.fit(y = y, x = cbind(1, x))}
#' predFunLM = function(mod, x) {cbind(1,x) %*% mod$coef}
#' R2lm = oosse(y = Brassica$Pheno$Leaf_8_width, x = Brassica$Expr[, 1:10],
#' fitFun = fitFunLM, predFun = predFunLM, nFolds = 10)
#' buildConfInt(R2lm)
#' buildConfInt(R2lm, what = "MSE")
#' buildConfInt(R2lm, what = "MST")
#' @references
#'    \insertAllCited{}
buildConfInt = function(oosseObj, what = names(oosseObj)[1], conf = 0.95){
    stopifnot(conf >0, conf <1)
    what = match.arg(what, choices = choices <- c("R2", "MSE", "MST", "BrierScore", "BrierSkillScore", "ReferenceBrierScore",
                                       "ModelMisclassRate", "ReferenceMisclassRate","PeirceSkillScore", "MisclassifcationSkillScore",
                                       "ReferenceLogLoss", "ModelLogLoss", "McFaddenSkillScore"))
    bounds <- c((1-conf)/2, conf + (1-conf)/2)
    if(what %in% setdiff(choices, "MST")){
        zQuants = qnorm(bounds)
        obj = oosseObj[[what]]
        ci = with(oosseObj, obj["Estimate"] + obj["StandardError"]*zQuants)
        if(what %in% c("R2", "BrierSkillScore", "PeirceSkillScore", "MisclassifcationSkillScore", "McFaddenSkillScore",
                       "ModelMisclassRate", "ReferenceMisclassRate", "BrierScore", "ReferenceBrierScore")){
            ci[2] = min(ci[2], 1) #Truncate at 1
        } else if(what %in% c("MSE", "ModelMisclassRate", "ReferenceMisclassRate", "BrierScore",
                              "ReferenceLogLoss", "ModelLogLoss", "ReferenceBrierScore")){
            ci[1] = max(ci[1], 0) #Truncate at 0
        }
    } else if (what == "MST"){
        obj = oosseObj[[what]]
        ci = with(oosseObj, obj["Estimate"]*((n-1)/qchisq(p = bounds[2:1], df = (n-1))))
    }
    names(ci) = paste0(bounds*100, "%")
    return(ci)
}