context("Check input of RsquaredSE function")
test_that("RsquaredSE throws errors when incorrect input is provided", {
    expect_error(RsquaredSE(MSE = 3, margVar = 4, SEMSE = 0.4, n = 0, corMSEMST = 0.75))
    expect_error(RsquaredSE(MSE = -3, margVar = 4, SEMSE = 0.4, n = 50, corMSEMST = 0.75))
    expect_error(RsquaredSE(MSE = 3, margVar = -4, SEMSE = 0.4, n = 50, corMSEMST = 0.75))
    expect_error(RsquaredSE(MSE = 3, margVar = 4, SEMSE = -0.4, n = 50, corMSEMST = 0.75))
    expect_error(RsquaredSE(MSE = 3, margVar = 4, SEMSE = 0.4, n = 50, corMSEMST = 2))
    })
test_that("RsquaredSE works as expected with correct input", {
    expect_length(RsquaredSE(MSE = 3, margVar = 4, SEMSE = 0.4, n = 50, corMSEMST = 0.75), 2)
})
