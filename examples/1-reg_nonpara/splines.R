
rm(ls = list())

require(splines)

datf <- readRDS("./data/soenderborg_2day.RDS")

li_splines <- reg_splines(
  li = c(x = datf$Te, y = datf$P),
  df = 3, degree = 1, whe_intercept = T
)

plot_splines(li_splines)

## ----Degrees of freedom (df)-----------------------------------
## Build a model using base splines
##
## What should the df be?
##
fit <- lm(P ~ bs(Te, df=3), datf)
## We get a coefficient for each base spline
summary(fit)
## Plot the relation between external temperature and heat load
plot(datf$Te, datf$P)
## We need to use predict to see the estimated function
lines(xseq, predict(fit, newdata=data.frame(Te=xseq)))



## ----Intercept--------------------------------------------------
## Use an intercept in lm() or in bs() gives nearly the same result
##   using same number of parameters in the model
fit <- lm(P ~ bs(Te, df=3), datf)
summary(fit)
## Plot the relation between external temperature and heat load
plot(datf$Te, datf$P)
## We need to use predict to see the estimated function
lines(xseq, predict(fit, newdata=data.frame(Te=xseq)))
##
fit <- lm(P ~ 0 + bs(Te, df=4, intercept=TRUE), datf)
summary(fit)
## We need to use predict to see the estimated function
lines(xseq, predict(fit, newdata=data.frame(Te=xseq)), col=2)
