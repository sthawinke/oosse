context("Formatting seconds")
test_that("Seonds are properly formatted", {
    expect_false(grepl("minutes", formatSeconds(50)))
    expect_true(grepl("minutes", formatSeconds(100)))
    expect_false(grepl("hours", formatSeconds(3000)))
    expect_true(grepl("hours", formatSeconds(4000)))
})
