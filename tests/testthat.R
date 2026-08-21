library(testthat)
library(oosse)
nCores <- 2 # For CRAN build max 2
library(BiocParallel)
if (.Platform$OS.type == "unix") {
    # On unix-based systems, use MulticoreParam
    register(MulticoreParam(nCores))
} else {
    # On windows, use makeCluster
    library(doParallel)
    Clus <- makeCluster(nCores)
    registerDoParallel(Clus)
    register(DoparParam(), default = TRUE)
}
fitFunTest <- function(y, x) {
  lm.fit(y = y, x = cbind(1, x))
}
predFunTest <- function(mod, x) {
  cbind(1, x) %*% mod$coef
}
fitFunBin <- function(y, x) {
  glm.fit(y = y, x = cbind(1, x), family = binomial(), control = glm.control(maxit = 100))
}
predFunBin <- function(mod, x) {
  expit(cbind(1, x) %*% mod$coef)
}
library(nnet)
fitFunMult <- function(y, x) {
  multinom(y ~ x, trace = FALSE)
}
predFunMult <- function(mod, x) {
  tmp <- tcrossprod(cbind(1, x), coef(mod))
  num <- 1 + rowSums(et <- exp(tmp))
  cbind(1 / num, et / num)
}
test_check("oosse")
