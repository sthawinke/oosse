library(testthat)
library(oosse)
fitFunTest = function(y, x){lm.fit(y = y, x = cbind(1, x))}
predFunTest = function(mod, x) {cbind(1,x) %*% mod$coef}
fitFunBin = function(y, x){glm.fit(y = y, x = cbind(1, x), family = binomial(), control = glm.control(maxit = 100))}
predFunBin = function(mod, x) {expit(cbind(1,x) %*% mod$coef)}
library(nnet)
fitFunMult = function(y, x){multinom(y ~ x, trace = FALSE)}
predFunMult = function(mod, x) {predict(mod, newdata = cbind(1,x), type = "probs")}
test_check("oosse")
