library(testthat)
library(oosse)
library(BiocParallel)
nCores = 2 # For CRAN build max 2
param <- if (.Platform$OS.type == "unix") {
    # On unix-based systems (linux and macOS), use MulticoreParam
    MulticoreParam(nCores, stop.on.error = FALSE)
} else {
    # On windows, build vignette without multithreading
    SerialParam(stop.on.error = FALSE)
}
register(param)
test_check("oosse")
