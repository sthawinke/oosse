
This repository demonstrates the use of the *oosse* package for
estimating out-of-sample R² and its standard error through resampling
algorithms of the [corresponding
article](https://arxiv.org/abs/2302.05131). In this readme file, we
provide installation instructions and basic usage examples, for more
information and options see the package vignette and the help files.

# Installation instructions

The *oosse* package can be installed from CRAN as:

``` r
install.packages("oosse")
```

Alternatively, the latest (devel) version can be installed from github
as follows.

``` r
library(devtools)
install_github("sthawinke/oosse")
```

# Illustration

The *R2oosse* function works with any pair of fitting and prediction
functions. Here we illustrate a number of them, but any prediction
function implemented in R can be used. The built-in dataset *Brassica*
is used, which contains *rlog*-transformed gene expression measurements
for the 1,000 most expressed genes in the *Expr* slot, as well as 5
outcome phenotypes in the *Pheno* slot.

``` r
library(oosse)
data(Brassica)
```

## Regularised linear models

As first example, we use the *cv.glmnet* function from the *glmnet*
package, which includes internal cross-validation for tuning the penalty
parameter. Following custom function definitions are needed to fit in
with the naming convention of the *oosse* package.

The fitting model must accept at least an outcome vector *y* and a
regressor matrix *x*:

``` r
fitFunReg = function(y, x, ...) {cv.glmnet(y = y, x = x, ...)}
```

The predictive model must accept arguments *mod* (the fitted model) and
*x*, the regressor matrix for a new set of observations.

``` r
predFunReg = function(mod, x, ...){predict(mod, newx = x)}
```

Now that these functions have been defined, we apply the prediction
model for leaf_8\_width using the LASSO. Multithreading is used
automatically using the *BiocParallel* package. Change the following
setup depending on your system.

``` r
library(BiocParallel)
nCores = 10
register(MulticoreParam(nCores))
```

Now estimate $R^2$ while passing on the cluster object, also an estimate
of the computation time is given.

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loaded glmnet 4.1-6

``` r
R2pen = R2oosse(y = Brassica$Pheno$Leaf_8_width, x = Brassica$Expr[, seq_len(1e2)],
               fitFun = fitFunReg, predFun = predFunReg, alpha = 1) #Lasso model
```

    ## Fitting and evaluating the model once took 0.07 seconds.
    ## You requested 200 repeats of 10-fold cross-validation with 10 cores, which is expected to last for roughly
    ## 2 minutes and 27.6 seconds

Estimates and standard error of the different components are now
available.

``` r
#R2
R2pen$R2
```

    ##         R2       R2SE 
    ## 0.62742380 0.09900512

``` r
#MSE
R2pen$MSE
```

    ##      MSE    MSESE 
    ## 2.130870 0.384306

``` r
#MST
R2pen$MST
```

    ##      MST    MSTSE 
    ## 5.719286 1.035600

Also confidence intervals can be constructed:

``` r
# R2
buildConfInt(R2pen)
```

    ##      2.5%     97.5% 
    ## 0.4333773 0.8214703

``` r
#MSE, 90% confidence interval
buildConfInt(R2pen, what = "MSE", conf = 0.9)
```

    ##       5%      95% 
    ## 1.498743 2.762997

``` r
#MST
buildConfInt(R2pen, what = "MST")
```

    ##     2.5%    97.5% 
    ## 4.129867 8.446729

By default, cross-validation (CV) is used to estimate the MSE, and
nonparametric bootstrapping is used to estimate the correlation between
MSE and MST estimators. Other parameters can be supplied though,
e.g. for bootstrap .632 estimation of the MSE and jackknife estimation
of the correlation:

``` r
R2penBoot = R2oosse(y = Brassica$Pheno$Leaf_8_width, x = Brassica$Expr[, seq_len(1e2)],
                     methodMSE = "bootstrap", methodCor = "jackknife", fitFun = fitFunReg,
                        predFun = predFunReg, alpha = 1, nBootstraps = 1e2)#Lasso model
```

    ## Fitting and evaluating the model once took 0.06 seconds.
    ## You requested 100 .632 bootstrap instances with 10 cores, which is expected to last for roughly
    ## 37.12 seconds

## Random forest

As a second example we use a random forest as a prediction model. We use
the implementation from the *randomForest* package.

``` r
library(randomForest)
```

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

``` r
fitFunrf = function(y, x, ...){randomForest(y = y, x, ...)}
predFunrf = function(mod, x, ...){predict(mod, x, ...)}
R2rf = R2oosse(y = Brassica$Pheno$Leaf_8_width, x = Brassica$Expr[, seq_len(1e2)],
                 nFolds = 5, cvReps = 1e2, nBootstrapsCor = 30,
                    fitFun = fitFunrf, predFun = predFunrf)
```

    ## Fitting and evaluating the model once took 0.16 seconds.
    ## You requested 100 repeats of 5-fold cross-validation with 10 cores, which is expected to last for roughly
    ## 41.34 seconds

``` r
R2rf$R2
```

    ##        R2      R2SE 
    ## 0.6758210 0.1106841
