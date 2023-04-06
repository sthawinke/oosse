context("Check for positive definiteness")
test_that("Positive definiteness is correctly called", {
    expect_false(isPD(matrix(c(1,2,2,1),2,2)))
    expect_true(isPD(matrix(c(1,0.5,0.5,1),2,2)))
})
