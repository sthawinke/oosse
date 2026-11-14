library(testthat)
library(oosse)
library(BiocParallel)
nCores = 2 # For CRAN build max 2
library(BiocParallel)
if(.Platform$OS.type == "unix"){
    #On unix-based systems, use MulticoreParam
    register(MulticoreParam(nCores))
} else {
    #On windows, use makeCluster
    library(doParallel)
    Clus = makeCluster(nCores)
    registerDoParallel(Clus)
    register(DoparParam(), default = TRUE)
}
test_check("oosse")
