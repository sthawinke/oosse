context("Input components")
n = 50;p=3
y = rnorm(n)
x = matrix(rnorm(n*p),n,p)
colnames(x) = paste0("Var", seq_len(p))
fitFunTest = function(y, x){lm.fit(y = y, x = cbind(1, x))}
predFunTest = function(mod, x) {cbind(1,x) %*% mod$coef}
test_that("oosse works as expected when correct input is provided", {
    expect_message(R2objCV <- oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest))
    expect_message(R2objCV <- oosse(y = y, x = x[, 1, drop = TRUE], predFun = predFunTest, fitFun = fitFunTest)) # Vector input
    expect_message(R2objCVjn <- oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest, methodCor = "jackknife"))
    expect_message(R2objBoot <- oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest,
                                        methodLoss = "bootstrap"))
    expect_message(R2objBootJn <- oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest,
                                        methodLoss = "bootstrap", methodCor = "jackknife"))
    expect_warning(R2objCV <- oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest, cvReps = 20))
    expect_silent(oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest, printTimeEstimate = FALSE))
})
fitFunBroken = function(y, x){lm.fit(y = y, x = rbind(1, x))}
predFunBroken = function(mod, x) {rbind(1,x) %*% mod$coef}
test_that("oosse throws an error when incorrect input is provided", {
    expect_error(R2objCV <- oosse(y = y, x = as.data.frame(x), predFun = predFunDf, fitFun = fitFunDf)) # Dataframe input
    expect_error(oosse(y = x, x = x, predFun = predFunTest, fitFun = fitFunTest))
    expect_error(oosse(y = y, x = as.data.frame(x), predFun = predFunTest, fitFun = fitFunTest))
    expect_error(oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunBroken))
    expect_error(oosse(y = y, x = x, predFun = predFunBroken, fitFun = fitFunTest))
    expect_error(oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest, methodLoss = "LOOCV"))
    expect_error(oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest, methodCor = "parametric"))
    expect_error(oosse(y = y, x = x, predFun = predFunTest, fitFun = 1))
    expect_error(oosse(y = y, x = x, predFun = 1, fitFun = fitFunTest))
    expect_error(R2objCV <- oosse(y = y, x = x, predFun = predFunTest, fitFun = fitFunTest, nFolds = 1)) #Only one CV fold
    })
