context("Check input of skillScoreSE function")
test_that("skillScoreSE throws errors when incorrect input is provided", {
    expect_error(skillScoreSE(MSE = 3, margVar = 4, SEMSE = 0.4, n = 0, corMSEMST = 0.75))
    expect_error(skillScoreSE(MSE = -3, margVar = 4, SEMSE = 0.4, n = 50, corMSEMST = 0.75))
    expect_error(skillScoreSE(MSE = 3, margVar = -4, SEMSE = 0.4, n = 50, corMSEMST = 0.75))
    expect_error(skillScoreSE(MSE = 3, margVar = 4, SEMSE = -0.4, n = 50, corMSEMST = 0.75))
    expect_error(skillScoreSE(MSE = 3, margVar = 4, SEMSE = 0.4, n = 50, corMSEMST = 2))
    })
test_that("skillScoreSE works as expected with correct input", {
    expect_length(skillScoreSE(MSE = 3, margVar = 4, SEMSE = 0.4, n = 50, corMSEMST = 0.75), 2)
})
