
rm(list = ls())
setwd("~/GitHub/MatrixTSA")
sapply(
  dir("./examples_source/prediction/functions", full.names = TRUE), source
  )
li_data <- readRDS("./data/data_soenderborg.RDS")

## Learn how the data is setup -------------------------------------------------

## The observed ambient temperature
li_data$Ta
## "t" is time
plot(li_data$t, li_data$Ta, type = "l")

## Its a vector
class(li_data$Ta)

## We also have Numerical Weather Forecasts of ambient temperature

## Arranged in a matrix
class(li$Tanwp)
dim(li_data$Tanwp)
names(li_data$Tanwp)

## See the forecast available at i
i <- 100
## The time
li_data$t[i]
## The forecast
li_data$Tanwp[100, ]

## Plot the observations and the k = 1 step ahead forecast
plot(li_data$t, li_data$Ta, type = "l")
lines(li_data$t, li_data$Tanwp$k1, col = 2)

## Make a scatter plot
plot(li_data$Ta, li_data$Tanwp$k1)

## Also of the k = 24 hour forecast
plot(li_data$Ta, li_data$Tanwp$k24)

## Wuups, we need to lag the NWPs to match in time
## See how lag_vector() works
x <- c(0,0,1,2,3,0,0)
lag_vector(x, 1)
lag_vector(x, 2)

## Lag the NWPs to match in time
plot(li_data$Ta, lag_vector(li_data$Tanwp$k1, 1))
plot(li_data$Ta, lag_vector(li_data$Tanwp$k24, 24))

## Which seems to be most accurate k = 1 or 24 steps ahead?
##----------------------------------------------------------------



##----------------------------------------------------------------
## Now lets make a model and calculate forecasts

## Make a data.frame with synced observations and NWPs
## Take Ph4, which is the load from House 4

## Make the k = 1 steps ahead "design matrix"
datf <- data.frame(t = li_data$t, Ph = li_data$Ph4, Ta = lag_vector(li_data$Tanwp$k1,1), G = lag_vector(li_data$Gnwp$k1,1))

## Make a training set, first 3 month, and a test set
## Just keep the indexes
itrain <- which(per("2010-09-01", datf$t, "2010-12-01"))
itest <- which(per("2010-12-01", datf$t, "2011-01-01"))

## Fit a linear model on the training set
fit <- lm(Ph ~ Ta + G, datf[itrain, ])

## Are the coefficients significant?
summary(fit)

## Predict
datf$Ph_hat_lm <- predict(fit, datf)

## Plot the test set
plot(datf$t[itest], datf$Ph[itest], type = "l")
lines(datf$t[itest], datf$Ph_hat_lm[itest], col = 2)

## The score
rmse(datf$Ph[itest] - datf$Ph_hat_lm[itest])
##----------------------------------------------------------------


