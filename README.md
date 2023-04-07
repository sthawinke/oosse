
This repository demonstrates the use of the *oosse* package for
estimating the out-of-sample R² and its standard error through
resampling algorithms of the . In this readme file, we provide
installation instructions and basic usage examples, for more information
and options see the package vignette and the help files.

# Installation instuctions

As soon as the package is available on CRAN:

``` r
install.packages("oosse") #When available on CRAN
```

Installation from github

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

Now estimate the $R^2$, also an estimate of the computation time is
given.

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loaded glmnet 4.1-6

``` r
R2pen = R2oosse(y = Brassica$Pheno$Leaf_8_width, x = Brassica$Expr[, seq_len(2e2)], 
                nFolds = 5, cvReps = 1e2, nBootstrapsCor = 30,
               fitFun = fitFunReg, predFun = predFunReg, alpha = 1) #Lasso model
```

    ## Fitting and evaluating the model once took 0.27 seconds.
    ## You requested 100 repeats of 5-fold cross-validation with 10 cores, which is expected to last for
    ## 1 minutes and 3.78 seconds

Estimates and standard error of the different components are now
available.

``` r
#R2
R2pen$R2
```

    ##         R2       R2SE 
    ## 0.62964397 0.09541882

``` r
#MSE
R2pen$MSE
```

    ##       MSE     MSESE 
    ## 2.1181720 0.4098121

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
    ## 0.4426265 0.8166614

``` r
#MSE, 90% confidence interval
buildConfInt(R2pen, what = "MSE", conf = 0.9)
```

    ##  5% 95% 
    ##  NA  NA

By default, cross-validation (CV) is used to estimate the MSE, and
nonparametric bootstrapping is used to estimate the correlation between
MSE and MST estimators. Other parameters can be supplied though,
e.g. for bootstrap .632 estimation of the MSE and jackknife estimation
of the correlation:

``` r
R2penBoot = R2oosse(y = Brassica$Pheno$Leaf_8_width, x = Brassica$Expr[, seq_len(2e2)],
                     methodMSE = "bootstrap", methodCor = "jackknife", fitFun = fitFunReg,
                        predFun = predFunReg, alpha = 1, nBootstraps = 1e2)#Lasso model
```

    ## Fitting and evaluating the model once took 0.18 seconds.
    ## You requested 100 .632 bootstrap instances with 10 cores, which is expected to last for
    ## 2 minutes and 11.56 seconds

## Support vector machine

As a second example we use a support vector machine as a prediction
model. We use the implementation from the *e1071* package.

``` r
library(e1071)
    fitFunSvm = function(y, x, ...){svm(y = y, x, ...)}
    predFunSvm = function(mod, x, ...){predict(mod, x, ...)}
    R2svm = R2oosse(y = Brassica$Pheno$Leaf_8_width, x = Brassica$Expr, cvReps = 1e2,
                        fitFun = fitFunSvm, predFun = predFunSvm)
```

    ## Fitting and evaluating the model once took 0.05 seconds.
    ## You requested 100 repeats of 10-fold cross-validation with 10 cores, which is expected to last for
    ## 50.67 seconds

``` r
    R2svm$R2
```

    ##        R2      R2SE 
    ## 0.5428769 0.1381585
