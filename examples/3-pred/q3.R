
rm(list = ls())

library(tidyverse)
library(tidymodels)
library(magrittr)
library(tsibble)
library(ggplot2)
library(bbplot)
library(purrr)
library(lubridate)
library(stringr)
library(splines)
setwd("~/GitHub/tidynamics")
sapply(
  dir("./examples/3-pred/funcs", full.names = TRUE), source
)

#### 0, Data ####
li <- readRDS("./data/data_soenderborg.RDS")

## Change the column names in `li$Gnwp` like "k1" to "t1"
for (i in names(li$Gnwp)) {
  li$Gnwp[paste0("t", str_sub(i, 2, -1))] = li$Gnwp[i]
}

#' Get the first character in the string
str_sub_1 <- function(chr){
  i <- NA
  if (str_sub(chr, 1, 1) == "k") {
    i <- "p_temp"
  } else {
    i <- "p_g"
  }
  return(i)
}

#' Get the value of step from the column names like "k1"
get_ahead <- function(chr){
  return(strtoi(str_sub(chr, 2, -1)))
}

## Get the forcast of ambient temperature and solar radiation, measurement of
##   ambient temp, solar radiation and heat load in house 4
ti <-
  as_tibble(
    cbind(
      data.frame(
        "t" = li$t, "s" = 1:length(li$t), "temp" = li$Ta, "g" = li$G,
        "ph" = li$Ph4
      ),
      li$Tanwp, li$Gnwp[, 50:98]
    )
  ) %>%
  gather(-t, -s, -temp, -g, -ph, key = "ahead_chr", value = "pred") %>%
  mutate(whi = map_chr(ahead_chr, str_sub_1)) %>%
  mutate(ahead = map_int(ahead_chr, get_ahead)) %>%
  mutate(w = s + ahead) %>%
  select(w, ahead, temp, g, ph, s, t, whi, pred) %>%
  arrange(w, ahead) %>%
  spread(key = whi, value = pred) %>%
  select(w, ahead, temp, p_temp, g, p_g, ph, s, t) %>%
  mutate(hour = hour(t)) %>%
  print()

#### 1, Linear Reg ####

## Apply a low-pass filter on the 1 step ahead ambient temperature
split_a1 <- ti %>%
  filter(ahead == 1) %>%
  mutate(p_temp_lp = lp_vector(.$p_temp, a1 = 0.99)) %>%
  mutate(hour = hour(.$t)) %>%
  select(w, p_temp, p_temp_lp, p_g, ph, s, t) %>%
  initial_split(0.6)

## Define the recipe with original forecasted ambient temperature
recipe_a1 <- split_a1 %>%
  training() %>%
  recipe(ph ~ p_temp + p_temp_lp + p_g) %>%
  # step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep() %>%
  summary()

dm_test_a1 <-
  recipe_a1 %>%
  bake(testing(split_a1))

dm_train_a1 <-
  juice(recipe_a1)

#### 2, Simple Model ####

## Linear regression using original forecast data of ambient temp and radiation
mod_lm_a1_1 <-
  linear_reg() %>%
  set_engine("lm") %>%
  fit(ph ~ p_temp + p_g, data = dm_train_a1)

## Predict the heat output using the fitted model and show the
##   validation result
dm_test_a1 %>%
  bind_cols(predict(mod_lm_a1_1, .)) %>%
  metrics(truth = ph, estimate = .pred)

dm_test_a1 %>%
  bind_cols(predict(mod_lm_a1_1, .), "t" = testing(split_a1)$t) %>%
  gather(ph, .pred, key = "type", value = "temp") %>%
  ggplot() +
    geom_line(aes(x = t, y = temp, color = type)) +
    labs(
      title = "Obs and Linear Reg Pred of Heat Load in House 4",
      subtitle = "with 1-step forecasted ambient temp and radiation as input",
      x = "Time (Hour)", y = "Temp (Celsius D)"
      )

#### 3, LP Filter ####
## We know that there are dynamics from Ta to Ph
## The low-pass filter function is defined in "functions/lp_vector.R"
## Linear regression using original forecast data of ambient temp and radiation
mod_lm_a1_2 <-
  linear_reg() %>%
  set_engine("lm") %>%
  fit(ph ~ p_temp_lp + p_g, data = dm_train_a1)

## Predict the heat output using the fitted model and show the
##   validation result
dm_test_a1 %>%
  bind_cols(predict(mod_lm_a1_2, .)) %>%
  metrics(truth = ph, estimate = .pred)

dm_test_a1 %>%
  bind_cols(predict(mod_lm_a1_2, .), "t" = testing(split_a1)$t) %>%
  gather(ph, .pred, key = "type", value = "temp") %>%
  ggplot() +
  geom_line(aes(x = t, y = temp, color = type)) +
  labs(
    title = "Obs and Linear Reg Pred of Heat Load in House 4",
    subtitle = paste0(
      "with LP filtered 1-step forecasted ambient temp and ",
      "original 1-step radiation as input"
      ),
    x = "Time (Hour)", y = "Temp (Celsius D)"
  )

#### 4, Tune the Low-Pass Coeff ####

#' Get a set of splits from cross validation
#' @return list of splits
get_splits <- function(coef_temp, coef_g, ti, ahead = 1) {
  splits <- ti %>%
    filter(ahead == ahead) %>%
    mutate(p_temp_lp = lp_vector(.$p_temp, a1 = coef_temp)) %>%
    mutate(p_g_lp = lp_vector(.$p_g, a1 = coef_g)) %>%
    mutate(hour = hour(.$t)) %>%
    select(s, p_temp_lp, p_g_lp, ph, t, hour) %>%
    vfold_cv(v = 10, repeats = 10) %>%
    {unlist(.$splits, use.names = FALSE)} %>%
    return()
}

## Test `get_splits`
head(get_splits(0.9, 0.9, ti))

#' Get recipe from split
get_rec <- function(split) {
  rec <-
    training(split) %>%
    recipe(ph ~ p_temp_lp + p_g_lp) %>%
    # step_corr(all_predictors()) %>%
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) %>%
    prep()

  return(rec)
}

## Test `get_rec`
get_rec(get_splits(0.9, 0.9, ti)[[1]])

#' Evaluate the recipe
get_rmse <- function(recipe, split) {

  dm_test <- bake(recipe, testing(split))
  dm_train <- juice(recipe)

  mod_lm <- linear_reg() %>%
    set_engine("lm") %>%
    fit(ph ~ ., data = dm_train)

  ti_result <- bind_cols(dm_test, predict(mod_lm, dm_test))

  rmse <- metrics(data = ti_result, truth = ph, estimate = .pred) %>%
    `[[`(1, 3)

  return(rmse)
}

validate_para <- function(coef_temp, coef_g, ti) {
  split <- get_splits(coef_temp, coef_g, ti)[[1]]
  rec <- get_rec(split)
  rmse <- get_rmse(recipe, split)
}

#' To cross validate the parameters
cv_para <- function(para, ti) {
  coef_temp = para[1]
  coef_g = para[2]
  rmse_mean <- mean(map_dbl(get_splits(coef_temp, coef_g, ti), validate_para))
  return(rmse_mean)
}

test_para

## Optimize the choice of low-pass filtering coefficients
result <-
  optim(
    c(coef_temp = 0.98, coef_g = 0.98), cv_para, lower = c(0.3, 0.1),
    upper = c(0.999, 0.999), method = "L-BFGS-B", ti = ti
    ) %>%
  print()

## Result:
##   coef_temp = 0.9696484
##   coef_g = 0.9652321
##   value = 0.6958801

#### 5, Best Model ####

split_a1_2 <-
  ti %>%
  filter(ahead == 1) %>%
  mutate(p_temp_lp = lp_vector(.$p_temp, a1 = 0.9696484)) %>%
  mutate(p_g_lp = lp_vector(.$p_g, a1 = 0.9652321)) %>%
  mutate(hour = hour(.$t)) %>%
  select(s, p_temp_lp, p_g_lp, ph, t) %>%
  initial_time_split(0.6)

recipe_a1_2 <-
  training(split_a1_2) %>%
  recipe(ph ~ p_temp_lp + p_g_lp) %>%
  # step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

dm_test_a1_2 <-
  recipe_a1_2 %>%
  bake(testing(split_a1_2))

dm_train_a1_2 <-
  juice(recipe_a1_2)

mod_lm_a1_3 <-
  linear_reg() %>%
  set_engine("lm") %>%
  fit(ph ~ p_temp_lp + p_g_lp, data = dm_train_a1_2)

dm_test_a1_2 %>%
  bind_cols(predict(mod_lm_a1_3, .)) %>%
  metrics(truth = ph, estimate = .pred)

dm_test_a1_2 %>%
  bind_cols(predict(mod_lm_a1_3, .), "t" = testing(split_a1_2)$t) %>%
  gather(ph, .pred, key = "type", value = "temp") %>%
  ggplot() +
  geom_line(aes(x = t, y = temp, color = type)) +
  labs(
    title = "Obs and Linear Reg Pred of Heat Load in House 4",
    subtitle = paste0(
      "with best LP filtered 1-step forecasted ambient temp and ",
      "best LP filtered 1-step radiation as input"
    ),
    x = "Time (Hour)", y = "Temp (Celsius D)"
  )

#### 6, Diurnal Curve as Input ####

#' Get the recipe with base splines of diurnal factor
get_rec_diurnal <- function(split) {
  recipe_a1 <-
    training(split) %>%
    recipe(ph ~ p_temp_lp + p_g_lp) %>%
    step_bs(hour, deg_free = 3) %>%
    # step_corr(all_predictors()) %>%
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) %>%
    prep()
  return(recipe_a1)
}

eval_cv_diurnal <- function(para, ti) {
  coef_temp = para[1]
  coef_g = para[2]
  ti_splits <- get_splits(coef_temp, coef_g, ti)
  rmse_mean <- mean(map_dbl(ti_splits, get_rmse))
  return(rmse_mean)
}

result <-
  optim(
    c(coef_temp = 0.98, coef_g = 0.98), eval_cv_2, lower = c(0.3, 0.1),
    upper = c(0.999, 0.999), method = "L-BFGS-B", ti = ti
  ) %>%
  print()











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
