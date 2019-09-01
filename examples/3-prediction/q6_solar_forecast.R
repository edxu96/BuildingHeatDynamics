## 0, Intialize ----
rm(list = ls())
setwd("~/GitHub/MatrixTSA")
sapply(
  dir("./examples_source/prediction/functions", full.names = TRUE), source
)
li_data <- readRDS("./data/data_soenderborg.RDS")

## Make a data.frame with synced observations and NWPs
k <- 24
datf <- data.frame(
  t = li_data$t, Ps = li_data$G, G = lag_vector(li_data$Gnwp[, pst("k", k)], k),
  tday = li_data$tday
  )
datf[ datf$tday <= 5 | 18 < datf$tday, -1] <- NA

## Divide the period into a training set and a test set
## Just keep the indexes
tstart <- "2011-03-01"
tstart_train <- "2011-04-10"
tend <- "2011-05-01"
datf <- datf[per(tstart, datf$t, tend), ]
itrain <- which(per(tstart, datf$t, tstart_train))
itest <- which(per(tstart_train, datf$t, tend))

## Plot (see functions/plotmulti.R)
plotmulti(datf, c("Ps|G"))

## See the scatter plot
plot(datf$G, datf$Ps)

## 1, Linear Regression ----
fit <- lm(Ps ~ G, datf[itrain, ])
abline(fit)

## RMSE on test set
datf$residuals_lm <- datf$Ps - predict(fit, datf)
rmse(datf$residuals_lm[itest])

plotmulti(datf[itrain[1:300], ], c("Ps|G", "residuals_lm"))

## Explore the residuals
boxplot(datf$residuals_lm ~ datf$tday)

## Divide data into morning and afternoon
## Only morning
Xmorning <- datf[asPlt(datf$t)$hour <= 12, ]
plot(Xmorning$G, Xmorning$Ps)
abline(lm(Ps ~ G, Xmorning))
##
Xafternoon <- datf[asPlt(datf$t)$hour > 12, ]
points(Xafternoon$G, Xafternoon$Ps, col = 2)
abline(lm(Ps ~ G, Xafternoon), col = 2)

## 2, Base spline model with lm ----
## Use the time of day to calculate base splines and multiply with G
## in this way the function between Ps and G can change conditional on time
## of day
fit_bs_lm <- lm(Ps ~ bs(tday, df = 10) * G, datf[itrain, ])
datf$residuals_bs_lm <- datf$Ps - predict(fit_bs_lm, datf)
rmse(datf$residuals_bs_lm[itest])

## See if that helps
boxplot(datf$residuals_lm ~ datf$tday, ylim = c(-400, 400))
boxplot(datf$residuals_bs_lm ~ datf$tday, ylim = c(-400, 400))

## Plot forecasts for the test set
tmp <- datf[itest, ]
plot(tmp$t, tmp$Ps, type = "l")
lines(tmp$t, tmp$Ps - tmp$residuals_lm, col = 2)
lines(tmp$t, tmp$Ps - tmp$residuals_bs_lm, col = 3)

## 3, Base spline with rls ----

#' Write an objective function calculating the score
obj <- function(prm, frml, data, k, ieval = 1:nrow(data)) {
  print(prm)
  ## Apply a low-pass filter on the input
  lambda <- prm[1]
  fit <- rls(as.formula(frml), lambda, data, k)
  ## Evaluate only on the ieval rows
  print(score <- rmse(fit$residuals[ieval]))
  return(score)
}

## Define the model formula
frml <- "Ps ~ bs(tday, df=10) * G"

## To have a "burn-in" period, then set ieval, such the first 5 days are not
## included in calculating the score
ieval <- itrain[-1:-(24 * 14)]

## Optimize the forgetting factor lambda
result <- optimize(obj, lower = 0.9, upper = 1, frml = frml, data = datf[itrain, ], k = k, ieval = ieval)

result$minimum

## Calculate the forecasts
fit <- rls(as.formula(frml), lambda = result$minimum, datf, k)
datf$residuals_bs_rls <- fit$residuals
## Plot
tmp <- datf[itest, ]
plot(tmp$Ps, type = "l")
lines(tmp$Ps - tmp$residuals_bs_rls, col = 2)
lines(tmp$Ps - tmp$residuals_bs_lm, col = 3)

rmse(datf$residuals_bs_rls[itest])
rmse(datf$residuals_bs_lm[itest])

## 4, Kernel model ----

## Wrap the leave-one-out in a function which returns the score
obj <- function(h, frml, data, k = k, ieval = 1:nrow(data), n_min = 10, return_yhat = FALSE) {
  ## Keep the output in yhat, only for ieval points
  yhat <- sapply(ieval, function(i) {
    ## Check if there is enough points to fit
    if ((i - k) < n_min) {
      return(NA)
    }
    if (is.na(data$tday[i])) {
      return(NA)
    }
    ## Only use values available k steps behind (otherwise future values would be used)
    ipast <- 1:(i - k)
    ## Only everything before the i'th observation will be included in the fit
    fit <- lm(as.formula(frml), data[ipast, ], weights = epanechnikov(data$tday[i], data$tday[ipast], h = h))
    ## Now predict for the i point (for the k'th horizon)
    predict(fit, newdata = data[i, ])
  })
  ##
  if (return_yhat) {
    return(yhat)
  } else {
    ## The score value
    nm <- all.vars(as.formula(frml))[1]
    val <- rmse(data[ieval, nm] - yhat)
    ##
    print(pst("h = ", h, ", val = ", val))
    ##
    return(val)
  }
}

frml <- "Ps ~ G"
h <- 3
ieval <- itrain[-1:-(24 * 14)]
obj(h, frml, datf, k, ieval)

result <- optimize(
  obj, lower = 0.9, upper = 4, frml = frml, data = datf[itrain, ],
  ieval = ieval, k = k
  )

result

Ps_hat <- obj(h = result$minimum, frml, datf, k, return_yhat = TRUE)
datf$residuals_kern <- datf$Ps - Ps_hat

## Compare
tmp <- datf[itest, ]
plot(tmp$Ps, type = "l")
lines(tmp$Ps - tmp$residuals_bs_rls, col = 2)
lines(tmp$Ps - tmp$residuals_bs_lm, col = 3)
lines(tmp$Ps - tmp$residuals_kern, col = 4)

boxplot(datf$residuals_kern ~ datf$tday, ylim = c(-400, 400))

rmse(datf$residuals_kern[itest])
rmse(datf$residuals_bs_lm[itest])
rmse(datf$residuals_bs_rls[itest])
rmse(datf$residuals_lm[itest])
