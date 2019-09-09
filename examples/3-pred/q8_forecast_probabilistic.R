
#### 1, Initiate ----
rm(list = ls())
setwd("~/GitHub/MatrixTSA")
require(verification)
require(quantreg)
require(splines)
sapply(
  dir("./examples_source/prediction/functions", full.names = TRUE), source
)
li_data <- readRDS("./data/data_soenderborg.RDS")

k <- 24
datf <- data.frame(t = li_data$t, Ps = li_data$G, G = lag_vector(li_data$Gnwp[, pst("k", k)], k), tday = li_data$tday)
datf[ datf$tday <= 5 | 18 < datf$tday, -1] <- NA
tstart <- "2011-03-01"
tstart_train <- "2011-04-10"
tend <- "2011-05-01"
datf <- datf[per(tstart, datf$t, tend), ]
itrain <- which(per(tstart, datf$t, tstart_train))
itest <- which(per(tstart_train, datf$t, tend))

## Plot (see functions/plotmulti.R)
plotmulti(datf, c("Ps|G"))

##### Quantile Reg ----
tau <- seq(0.05, by = 0.15)
fit <- rq(Ps ~ G, tau = tau, data = datf[itrain, ])
summary(fit)

## Predict the quantiles
Xrq <- predict(fit, datf)

## Plot
colnames(Xrq) <- pst("q", tau)
i <- per("2011-04-11", datf$t, "2011-04-14")
tmp <- cbind(datf[i, ], Xrq[i, ])
plotprob(list(tmp), nm.out = "Ps")

## CRPS score on test set, use the function from the verification package
tmp <- na.omit(cbind(datf$Ps[itest], Xrq[itest, ]))
crpsDecomposition(tmp[, 1], tmp[, -1])$CRPS

#### Quantitle Reg with Spline Base ----
## Use the time of day to calculate base splines and multiply with G
##   in this way the function between Ps and G can change conditional on time of day
## Fit a quantile regression model
fit <- rq(Ps ~ bs(tday, df = 5) * G, tau = tau, data = datf[itrain, ])
summary(Xrq)

## Predict the quantiles
Xrq <- predict(fit, datf)

## Plot
colnames(Xrq) <- pst("q", tau)
i <- per("2011-04-11", datf$t, "2011-04-14")
tmp <- cbind(datf[i, ], Xrq[i, ])
plotprob(list(tmp), nm.out = "Ps")

## CRPS score on test set, use the function from the verification package
tmp <- na.omit(cbind(datf$Ps[itest], Xrq[itest, ]))
crpsDecomposition(tmp[, 1], tmp[, -1])$CRPS
## ----------------------------------------------------------------



## ----------------------------------------------------------------
## (Optional) What about a kernel model?
## Below is pasted from "q6_solar_forecast.R":
## It is left as an optional exercise to "convert" the least squares into a quantile regression
## Need to replace lm() with rq(), and rmse with crpsDecomposition()
## and some other things on the way to make it work...

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

result <- optimize(obj, lower = 0.9, upper = 4, frml = frml, data = datf[itrain, ], ieval = ieval, k = k)

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
## ----------------------------------------------------------------
