library(testthat)
library(oosse)
fitFunTest = function(y, x){lm.fit(y = y, x = cbind(1, x))}
predFunTest = function(mod, x) {cbind(1,x) %*% mod$coef}
fitFunBin = function(y, x){glm.fit(y = y, x = cbind(1, x))}
expit = function(x){exp(x)/(1+exp(x))}
predFunBin = function(mod, x) {expit(cbind(1,x) %*% mod$coef)}
test_check("oosse")
