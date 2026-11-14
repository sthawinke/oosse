
# 1.0.3

- Confidence intervals for the MST provided based on chi-squared
  distribution.

# 1.0.4

- Change nomenclature to omit “the” from $R^2$.

# 1.0.6

- Cited publication in The American Statistician rather than preprint

# 1.0.7

- Bug fix when supplying vector rather than matrix of regressors

# 1.0.8

- Multithreading for windows machines in vignette

# 1.0.9

- Disallow data frames as input

# 1.0.10

- Set minimum of folds to 3 to prevent problems in CV

# 1.0.11

- Update Bates and own publication from preprints to published articles

# 1.0.12

 - Correct BugReports url
 - Change Brassica reference to published version
 
# 1.0.13

 - Switch to repeated CV also for estimating the correlation between MSE and MST in simpleCV()
 - Correct scaling factor for SE of MSE to (nOuterFolds-1)/nOuterFolds instead of nOuterFolds/(nOuterFolds-1), which was erring on the conservative side. This error was also present in the code of the original publication
 - Correct bug to propagate ... to all predFun and fitFun instances
 - Correct bug in fold assignment in estMSE()

