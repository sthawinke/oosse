context("Input components")
n = 100;p=4
y = rnorm(n)
x = matrix(rnorm(n*p),n,p)
fitFunTest = function(y, x){lm.fit(y = y, x = cbind(1, x))}
predFunTest = function(mod, x) {cbind(1,x) %*% mod$coef}
test_that("oosse works as expected when correct input is provided", {
    expect_message(R2objCV <- R2oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest))
    expect_message(R2objCVjn <- R2oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest, methodCor = "jackknife"))
    expect_message(R2objBoot <- R2oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest,
                                        methodMSE = "bootstrap"))
    expect_message(R2objBootJn <- R2oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest,
                                        methodMSE = "bootstrap", methodCor = "jackknife"))
    expect_warning(R2objCV <- R2oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest, cvReps = 20))
    expect_silent(R2oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest, printTimeEstimate = FALSE))
})
test_that("oosse throws an error when incorrect input is provided", {
    expect_error(R2oosse(y = x, x = x, predFun = predFunTest, fitFun = fitFunTest))
    expect_error(R2oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest, methodMSE = "LOOCV"))
    expect_error(R2oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest, methodCor = "parametric"))
    expect_error(R2oosse(y = y, x = x, predFun = predFunTest, fitFun = 1))
    expect_error(R2oosse(y = y, x = x, predFun = 1, fitFun = fitFunTest))
    })
