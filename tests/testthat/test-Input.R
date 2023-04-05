context("Input components")
library(nlme)
n <- 50 #Sample size
p <- 100 #Number of features
g <- 10 #Size of the grid
#Generate grid
Grid <- expand.grid("xLoc" = seq_len(g), "yLoc" = seq_len(g))
# Sample points from grid without replacement
GridSample <- Grid[sample(nrow(Grid), n, replace = FALSE),]
#Generate outcome and regressors
a <- rnorm(n)
b <- matrix(rnorm(p*n), n , p)
#Compile to a matrix
df <- data.frame("out" = a, "b" = b, GridSample)
# Define the correlation structure (see ?nlme::gls), with initial nugget 0.5 and range 5
corStruct <- corGaus(form = ~ xLoc + yLoc, nugget = TRUE, value = initValues <- c("range" = 5, "nugget" = 0.5))
test_that("Pengls works with other varible names", {
    expect_s3_class(pengls(data = df, outVar = "out", xNames = grep(names(df), pattern = "b", value =TRUE),
                           glsSt <- corStruct, nfolds = 5), "pengls")
    expect_s3_class(cv.pengls(data = df, outVar = "out", xNames = grep(names(df), pattern = "b", value =TRUE),
                           glsSt <- corStruct, nfolds = 5), "cv.pengls")
}
)
