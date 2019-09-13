## Solar Prediction
## Edward J. Xu
## Sept 11, 2019

rm(list = ls())

library(tidyverse)
library(lubridate)
library(splines)
library(ggplot2)
library(ggfortify)
library(rsample)
library(broom)

setwd("~/GitHub/tidynamics")
sapply(
  dir("./examples/3-pred/funcs", full.names = TRUE), source
)
li_data <- readRDS("./data/data_soenderborg.RDS")

li <- readRDS("./data/data_soenderborg_tidy.RDS")

ti_a24 <-
  li$pred %>%
  filter(ahead == 24) %>%
  left_join(li$time, by = "fo") %>%
  left_join(li$obs, by = "fo") %>%
  mutate(hour = as.numeric(hour(time))) %>%
  select(fo, hour, g, g.p, time)

ti_a24 %>%
  print()

splited_a24 <-
  ti_a24 %>%
  initial_split(0.6)

splited_a24 %>%
  training() %>%
  gather(g, g.p, key = "whi", value = "radia") %>%
  ggplot() +
    geom_point(mapping = aes(x = time, y = radia, color = whi)) +
    labs(
      title = "Obs and 24 Step Ahead Pred of Solar Radiation",
      x = "Time", y = "Solar Radiation (W)"
    )

splited_a24 %>%
  training() %>%
  ggplot() +
  geom_point(mapping = aes(x = g, y = g.p)) +
  geom_abline(aes(intercept = 0, slope = 1), color = "red") +
  labs(
    title = "Obs and 24 Step Ahead Pred of Solar Radiation",
    x = "Observed Solar Radiation (W)", y = "Predicted Solar Radiation (w)"
  )


#### 1, Linear Regression ####

li_mod <- list()

li_mod[[1]] <-
  splited_a24 %>%
  training() %>%
  {lm(g ~ g.p, .)}

rmse(li_mod[[1]]$residuals)

autoplot(li_mod[[1]])

## Divide data into morning and afternoon
li_mod[[2]] <-
  splited_a24 %>%
  training() %>%
  filter(hour <= 12) %>%
  {lm(g ~ g.p, .)}

li_mod[[3]] <-
  splited_a24 %>%
  training() %>%
  filter(hour > 12) %>%
  {lm(g ~ g.p, .)}

## Compare the performance of different models
li_mod %>%
  map_df(glance, .id = 'index') %>%
  arrange(logLik)

#### 2, Base spline model with lm ####

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




#### 3, Base spline with rls ####

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





#### 4, Kernel model ####

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

