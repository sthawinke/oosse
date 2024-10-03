context("Check input of skillScoreSE function")
test_that("skillScoreSE throws errors when incorrect input is provided", {
    expect_error(skillScoreSE(meanLoss = 3, margVar = 4, meanLossSE = 0.4, n = 0, corEst = 0.75))
    expect_error(skillScoreSE(meanLoss = -3, margVar = 4, meanLossSE = 0.4, n = 50, corEst = 0.75))
    expect_error(skillScoreSE(meanLoss = 3, margVar = -4, meanLossSE = 0.4, n = 50, corEst = 0.75))
    expect_error(skillScoreSE(meanLoss = 3, margVar = 4, meanLossSE = -0.4, n = 50, corEst = 0.75))
    expect_error(skillScoreSE(meanLoss = 3, margVar = 4, meanLossSE = 0.4, n = 50, corEst = 2))
    })
test_that("skillScoreSE works as expected with correct input", {
    expect_length(skillScoreSE(meanLoss = 3, margVar = 4, meanLossSE = 0.4, n = 50, corEst = 0.75), 2)
    expect_length(skillScoreSE(meanLoss = 3, refLoss = 4, refLossSE = .3, meanLossSE = 0.4, skillScore = "Brier", corEst = 0.75), 2)
})
