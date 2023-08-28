context("Confidence interval construction")
n = 50;p=3
x = matrix(rnorm(n*p),n,p)
y = rnorm(n, x %*% rnorm(p), sd = 0.5)
fitFunTest = function(y, x){lm.fit(y = y, x = cbind(1, x))}
predFunTest = function(mod, x) {cbind(1,x) %*% mod$coef}
R2objCV <- R2oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest, printTimeEstimate = FALSE)
test_that("confidence intervals are built with correct boundaries", {
    expect_silent(confIntR2 <- buildConfInt(R2objCV))
    expect_silent(confIntMSE <- buildConfInt(R2objCV, what = "MSE"))
    expect_silent(confIntMST <- buildConfInt(R2objCV, what = "MST", conf = 0.94))
    expect_true(all(confIntMSE > 0))
    expect_true(all(confIntMST > 0))
})
test_that("buildConfInt throws an error when incorrect input is provided", {
    expect_error(buildConfInt(R2objCV, what = "variance"))
    expect_error(buildConfInt(R2objCV, conf = 2))
    })
