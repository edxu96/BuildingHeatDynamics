## ----------------------------------------------------------------
rm(list = ls())
setwd("~/GitHub/MatrixTSA")
sapply(
  dir("./examples_source/prediction/functions", full.names = TRUE), source
)
li_data <- readRDS("./data/data_soenderborg.RDS")

## Make the k = 1 "design matrix"
datf <- data.frame(
  t = li_data$t, Ph = li_data$Ph4, Ta = lag_vector(li_data$Tanwp$k1, 1),
  G = lag_vector(li_data$Gnwp$k1, 1), tday = li_data$tday
  )

itrain <- which(per("2010-09-01", datf$t, "2010-12-01"))
itest <- which(per("2010-12-01", datf$t, "2011-01-01"))

## ----------------------------------------------------------------
## We know that there are dynamics from Ta to Ph

## The low-pass filter function is defined in "functions/lp_vector.R"

## Apply a low-pass filter on the input
datf$Ta_lp <- lp_vector(datf$Ta, a1 = 0.99)
## and plot the test set
plot(datf$t[itest], datf$Ta[itest], type = "l")
lines(datf$t[itest], datf$Ta_lp[itest], col = 2)

## Use it to make a model
## Fit a linear model on the training set
fit <- lm(Ph ~ Ta_lp + G, datf[itrain, ])

## Are the coefficients significant?
summary(fit)

## Predict and plot
datf$Ph_hat_lp1 <- predict(fit, datf)
##
plot(datf$t[itest], datf$Ph[itest], type = "l")
lines(datf$t[itest], datf$Ph_hat_lp1[itest], col = 2)

rmse(datf$Ph[itest] - datf$Ph_hat_lp1[itest])

## ----------------------------------------------------------------
## We have to tune the low-pass coefficient, do it
obj <- function(prm, frml, data) {
  ## Find the inputs to lowpass filter
  ## Just overwrite the column in data
  for (nm in names(prm)) {
    data[, nm] <- lp_vector(data[, nm], a1 = prm[nm])
  }
  ## Fit the model, ONLY on the training set
  fit <- lm(frml, data[itrain, ])
  ## Calculate the objective function
  print(val <- rmse(fit$residuals))
  ## The score value
  val <- rmse(fit$residuals)
  print(val)
  return(val)
}

frml <- "Ph ~ Ta + G"

obj(c(Ta = 0.98, G = 0.98), frml, datf[itrain, ])

result <- optim(
  c(Ta = 0.98, G = 0.98), obj, lower = c(0.3, 0.1), upper = c(0.999, 0.999),
  frml = frml, data = datf[itrain, ], method = "L-BFGS-B"
  )

result
## ----------------------------------------------------------------



## ----------------------------------------------------------------
## Now lets analyse the predictions

## Hmm, lets extend our objective function, such that it also can return the predictions
obj <- function(prm, frml, data, itrain, return_fit = FALSE) {
  ## Find the inputs to lowpass filter
  ## Just overwrite the column in data
  for (nm in names(prm)) {
    data[, nm] <- lp_vector(data[, nm], a1 = prm[nm])
  }
  ## Fit the model, ONLY on the training set
  fit <- lm(frml, data[itrain, ])
  ## Calculate the objective function
  print(val <- rmse(fit$residuals))
  ## Either return the fit and more
  if (return_fit) {
    return(list(
      val = val,
      fit = fit,
      yhat = predict(fit, data)
    ))
  } else {
    ## Or just return the score
    return(val)
  }
}

## It can be used in optim
result <- optim(
  c(Ta = 0.98, G = 0.98), obj, lower = c(0.3, 0.1), upper = c(0.999, 0.999),
  frml = frml, data = datf[itrain, ], method = "L-BFGS-B", itrain = itrain
  )

## But now we can also get the fit and the predictions
L <- obj(result$par, frml, datf, itrain, return_fit = TRUE)

summary(L$fit)

datf$Ph_hat_lpopt <- L$yhat

## Plot the predictions
plot(datf$t[itest], datf$Ph[itest], type = "l")
lines(datf$t[itest], datf$Ph_hat_lpopt[itest], col = 2)

## The score
rmse(datf$Ph[itest] - datf$Ph_hat_lpopt[itest])

## ----------------------------------------------------------------
## We miss something: a diurnal curve

## Use base splines
library(splines)

## We have the hour of the day
datf$tday

## Hey thats simple now! Add base splines to the formula
frml <- as.formula(Ph ~ bs(tday, df = 4) + Ta + G)

## Tune the low-pass coefficients
result <- optim(
  c(Ta = 0.98, G = 0.98), obj, lower = c(0.3, 0.1), upper = c(0.999, 0.999),
  frml = frml, data = datf[itrain, ], method = "L-BFGS-B"
  )

result

## Get the fit and the predictions
L <- obj(result$par, frml, datf, itrain, return_fit = TRUE)

summary(L$fit)

datf$Ph_hat_diur <- L$yhat

##
plot(datf$t[itest], datf$Ph[itest], type = "l")
lines(datf$t[itest], datf$Ph_hat_diur[itest], col = 2)

## The score, compare it to the "simpler" models
rmse(datf$Ph[itest] - datf$Ph_hat_diur[itest])
rmse(datf$Ph[itest] - datf$Ph_hat_lpopt[itest])
rmse(datf$Ph[itest] - datf$Ph_hat_lp1[itest])
## ----------------------------------------------------------------



## ----------------------------------------------------------------
## (Out of scope) We could use Fourier series as base functions

## Fourier series for one day of hourly values
x <- (0:23 + 0.5) / 24
n_harmonics <- 4
L <- lapply(1:n_harmonics, function(i) {
  val <- data.frame(sin(i * x * 2 * pi), cos(i * x * 2 * pi))
  names(val) <- paste0(c("sin_", "cos_"), i)
  return(val)
})
Xtmp <- do.call("cbind", L)
##
par(mfrow = c(2, 1))
plot(Xtmp$sin_1, type = "b")
for (i in 2:ncol(Xtmp)) {
  lines(Xtmp[, i], col = i, type = "b")
}
##
plot(Xtmp[, ncol(Xtmp)], type = "b")
## A linear combination can form any harmonic function
plot(apply(runif(n_harmonics * 2) * t(Xtmp), 2, sum), type = "b")

## How many harmonics make sense to maximum include when the period is in 24 steps?
## ----------------------------------------------------------------


## ----------------------------------------------------------------
## Fit a diurnal curve with Fourier series as base functions
tmp <- fs(datf$tday / 24, n_harmonics = 3)
tmp <- do.call("cbind", tmp)

Xfit <- cbind(datf, tmp)

frml <- as.formula(Ph ~ sin_1 + cos_1 + sin_2 + cos_2 + sin_3 + cos_3 + Ta + G)

## Tune the low-pass coefficients
result <- optim(c(Ta = 0.98, G = 0.98), obj, lower = c(0.3, 0.1), upper = c(0.999, 0.999), frml = frml, data = Xfit[itrain, ], method = "L-BFGS-B")

result

## But now we can also get the fit and the predictions
L <- obj(result$par, frml, Xfit, itrain, return_fit = TRUE)

summary(L$fit)

datf$Ph_hat_fs_diur <- L$yhat

##
plot(datf$t[itest], datf$Ph[itest], type = "l")
lines(datf$t[itest], datf$Ph_hat_fs_diur[itest], col = 2)

## The score, compare it to the "simpler" models
rmse(datf$Ph[itest] - datf$Ph_hat_fs_diur[itest])
rmse(datf$Ph[itest] - datf$Ph_hat_diur[itest])
rmse(datf$Ph[itest] - datf$Ph_hat_lpopt[itest])
rmse(datf$Ph[itest] - datf$Ph_hat_lp1[itest])
