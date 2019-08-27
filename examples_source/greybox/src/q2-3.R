# Edward J. Xu

rm(list = ls())

library(ctsmr)

files <- dir(
  "~/GitHub/MatrixTSA/examples_source/greybox/src/funcs",
  full.names = TRUE
  )
for (i in 1:length(files)) {
  source(files[i])
}
files <- dir(
  "~/GitHub/MatrixTSA/examples_source/greybox/src/models",
  full.names = TRUE
)
for (i in 1:length(files)) {
  source(files[i])
}
rm(files)
source("~/GitHub/MatrixTSA/examples_source/greybox/src/func.R")

datf <- get_datf()

## Run the parameter optimization for the first model
ctsmr_Ti <- Ti(datf)
## Analyze the results (If you want in RStudio not to have external plot
## windows, change "newdev", maybe the default value in "functions/analyzeFit.R")
analyzeFit(ctsmr_Ti, newdev=TRUE)

## Run the parameter optimization
ctsmr_TiTe <- TiTe(datf)
## Analyze the results
analyzeFit(ctsmr_TiTe)

## Run the parameter optimization
ctsmr_TiTh <- TiTh(datf)
## Analyze the results
analyzeFit(ctsmr_TiTh)

## Run the parameter optimization
ctsmr_TiTm <- TiTm(datf)
## Analyze the results
analyzeFit(ctsmr_TiTm)

## Run the parameter optimization
ctsmr_TiTs <- TiTs(datf)
## Analyze the results
analyzeFit(ctsmr_TiTs)
##----------------------------------------------------------------


##----------------------------------------------------------------
## Question 3:

## Which of the extensions have the highest likelihood?
ctsmr_TiTe$loglik
ctsmr_TiTh$loglik
ctsmr_TiTm$loglik
ctsmr_TiTs$loglik

## Perform a likelihood ratio test: lambda = lik(smallerModel)/lik(largerModel) ,
## where the smallerModel is submodel of the largerModel and lambda is chi2(f)
## distributed with f=dim(smallerModel)-dim(largerModel). Page 20 in Madsen2006.

test_logLik_ratio(ctsmr_TiTh, ctsmr_Ti)

##----------------------------------------------------------------
## PROCEED the forward selection
## The models which could be used in the next step are implemented in the following functions
## Fit different models extended from TiTh
ctsmr_TiThTe <- TiThTe(datf)
## Analyze the results
analyzeFit(ctsmr_TiThTe)

## Fit different models extended from TiTh
ctsmr_TiThTs <- TiThTs(datf)
## Analyze the results
analyzeFit(ctsmr_TiThTs)

## Fit different models extended from TiTh
ctsmr_TiThTm <- TiThTm(datf)
## Analyze the results
analyzeFit(ctsmr_TiThTm)

## Which one of the three extended models should we select?
## Take the one with the highest likelihood, as in previous step.
ctsmr_TiThTe$loglik
ctsmr_TiThTs$loglik
ctsmr_TiThTm$loglik

likRatioTest(ctsmr_TiThTe, ctsmr_TiTh)

## From here we should keep on extending the model, but for now no larger
## linear models are implemented here. From this points it is also likely that
## extensions to linear models compensate for non-linear or time-dependent
## effects. See the article "Identifying suitable models for heat dynamics",
## it is included in the .zip file, the performance (i.e. ACF(e_k) etc.)
## doesn't really change for models larger than TiThTe compared to the larger
## tested linear model. Hence we should rather look in residuals for non-linear
## or transformations of the inputs in order to model the effects which are not
## described well in the current model.

## The estimated HLC-value for the TiThTe model
i <- which(names(ctsmr_TiThTe$xm) %in% c("Rea","Rie"))
HLC <- 1 / sum(ctsmr_TiThTe$xm[i])
HLC * 1000 ## W/C
## The covariance for the two estimated R values
cov <- diag(ctsmr_TiThTe$sd[i]) %*% ctsmr_TiThTe$corr[i,i] %*% diag(ctsmr_TiThTe$sd[i])

## Calculate the uncertainty of the HLC value with a linear approximation to the covariance
## The Jacobian, the derived of the HLC-value with respect to each estimate in ctsmr_TiThTe$xm[i]
J <- t( sapply(1:length(i), function(ii,x){ -1/sum(x)^2 }, x = ctsmr_TiThTe$xm[i]) )
## The estimated variance of HLC
varHLC <- J %*% cov %*% t(J)
## and standard deviance
sdHLC <- sqrt(varHLC)
## Return the confidence interval
c(HLC-1.96*sdHLC,HLC+1.96*sdHLC)*1000


## Calculate the uncertainty of the HLC value with a simulation approach
## Needed for multivariate normal distribution simulation
require(MASS)
## Generate multivariate normal random values
Rsim <- mvrnorm(n=1000000,mu=ctsmr_TiThTe$xm[i],Sigma=cov)
## For each realization calculate the HLC-value
HLCsim <- 1/apply(Rsim,1,sum)
## Estimate the 2.5% and 97.5% quantiles of the simulated values as a confidence interval
quantile(HLCsim,probs=c(0.025,0.975))*1000
##----------------------------------------------------------------
