context("Input components")
n = 100;p=4
y = rnorm(n)
x = matrix(rnorm(n*p),n,p)
fitFun = function(y, x, id){lm.fit(y = y[id], x = cbind(1, x[id,]))}
predFun = function(mod, x) {cbind(1,x) %*% mod$coef}
R2obj = R2oosse(y = y, x = x, predFun = predFun, fitFun = fitFun)
