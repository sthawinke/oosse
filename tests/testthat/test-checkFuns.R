context("Input components")
fitFunTest = function(y, x){lm.fit(y = y, x = cbind(1, x))}
predFunTest = function(mod, x) {cbind(1,x) %*% mod$coef}
test_that("check functions return functions for good input functions", {
    expect_is(checkFitFun(fitFunTest), "function")
    expect_is(checkPredFun(predFunTest), "function")
})
test_that("check functions throw error when input is wrong", {
    expect_error(checkFitFun(predFunTest))
    expect_error(checkPredFun(fitFunTest))
})
