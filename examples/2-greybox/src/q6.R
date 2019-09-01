## Setup -------------------------------------------------------------------
rm(list = ls())

setwd("~/GitHub/MatrixTSA/examples_source/greybox/src")

library(lubridate)
library(ctsmr)
source("./func.R")
sapply(dir("funcs_2", full.names = TRUE), source)

pg <- get_pg()

## Averaging period Ts is in minutes
datf <- prepare_data(
  readSeries("X5", Ts = 10, nlags = NA), samples_after_Qi_step = 10
  )

plot_multi(
  datf, c("yTi", "Te", "Qi", "Gv", "stepQi"),
  ylab = c("Ti (c)", "Te (C)", "Qi (W)", "Gv (W/m2)", "stepQi")
  )

## Plot the different solar radiation measuresments
plot_multi(datf, c("sunElevation$", "Gh.1|Gv"))

## Set solar radiation to 0 when sun is below horizon
datf$Gv[ datf$sunElevation < 0 ] <- 0
datf$Gh.1[ datf$sunElevation < 0 ] <- 0

## Investigate how gA change ------------------------------------------------------

## Generate a new object of class ctsm
model <- ctsm$new()
## Add system equations and thereby also states
model$addSystem(dTi ~ (
  1 / (Ci * Riw) * (Tw - Ti) + gA / Ci * Gh + 1 / Ci * Qi) * dt +
    (1 + (stepQi * sigmalevel)) * exp(p11) * dw1
  )
model$addSystem(dTw ~ (
  1 / (Cw * Riw) * (Ti - Tw) + 1 / (Cw * Rwe) * (Te - Tw)) * dt +
    (1 + (stepQi * sigmalevel)) * exp(p22) * dw2
  )
model$addSystem(gA ~ exp(p33) * dw3)
## Set the names of the inputs
model$addInput(Te, Gh, Qi, stepQi)

## Set the observation equation: Ti is the state, yTi is the measured output
model$addObs(yTi ~ Ti)
## Set the variance of the measurement error
model$setVariance(yTi ~ exp(e11))

## Set the initial value (for the optimization) of the value of the state at
## the starting time point
model$setParameter(Ti = c(init = 35, lb = 10, ub = 45))
model$setParameter(Tw = c(init = 34, lb = 10, ub = 45))
model$setParameter(gA = c(init = 0.2, lb = 0.001, ub = 3))
## Set the initial value of the parameters for the optimization
model$setParameter(Ci = c(init = 1E5, lb = 1E4, ub = 1E7))
model$setParameter(Cw = c(init = 5E5, lb = 1E4, ub = 1E8))
model$setParameter(Riw = c(init = 0.1, lb = 1E-5, ub = 10))
model$setParameter(Rwe = c(init = 0.1, lb = 1E-5, ub = 10))
model$setParameter(p11 = c(init = 1, lb = -50, ub = 10))
model$setParameter(p22 = c(init = 1, lb = -50, ub = 10))
model$setParameter(p33 = c(init = 1, lb = -50, ub = 10))
model$setParameter(e11 = c(init = -1, lb = -50, ub = 10))
model$setParameter(sigmalevel = c(init = 1, lb = 0, ub = 100))

## Run the parameter optimization
fit <- model$estimate(data = datf, firstorder = pg$firstorder, threads = pg$threads)

## Check model
summary(fit, extended = TRUE)

## Plot the gA state
par(mfrow = c(3, 1))
val <- predict(fit)[[1]]
plot(datf$timedate, datf$Gv, ylab = "gA", xlab = "Time", bty = "n")
plot(datf$timedate, val$state$pred$gA, ylab = "gA", xlab = "Time", bty = "n")
plot(
  datf$sunAzimuth, val$state$pred$gA, ylab = "gA", bty = "n",
  xlab = expression("Sun Azimuth (" * degree * ")")
  )

# Splined gA curve --------------------------------------------------------

## Model using base splines
library(splines)

plot(datf$sunAzimuth, datf$sunElevation)
abline(h = 0)

## Inset the boundary angles (in radians) below.
azimuth_boundary <- c(1, 5.2)

## Create base splines
datf_bs <- as.data.frame(
  bs(
    datf$sunAzimuth, df = 4,
    Boundary.knots = c(azimuth_boundary[1], azimuth_boundary[2]),
    intercept = TRUE
    )
  )

## Name the columns and bind together with the data
names(datf_bs) <- pst("Gbs", names(datf_bs))
datf <- cbind(datf, datf_bs)

## Play around with the value of these parameters
gA <- c(0.7, 0.2, 1, 1.5)
# Plot spline function
with(
  subset(datf, day(datf$timedate) == 29), {
    plot((
      gA[1] * ifelse(sunElevation > 0, Gbs1, NA) +
      gA[2] * ifelse(sunElevation > 0, Gbs2, NA) +
      gA[3] * ifelse(sunElevation > 0, Gbs3, NA) +
      gA[4] * ifelse(sunElevation > 0, Gbs4, NA)),
      type = "l", ylab = "y"
      )
    }
  )


## Fit model with a spline gA-curve ------------------------------------------------

## Generate a new object of class ctsm
model <- ctsm$new()
## Add system equations and thereby also states
model$addSystem(dTi ~ (1 / (Ci * Riw) * (Tw - Ti) + 1 / Ci * Qi + Gv * (gA1 * Gbs1 + gA2 * Gbs2 + gA3 * Gbs3 + gA4 * Gbs4) / Ci) * dt + (1 + (stepQi * sigmalevel)) * exp(p11) * dw1)
model$addSystem(dTw ~ (1 / (Cw * Riw) * (Ti - Tw) + 1 / (Cw * Rwe) * (Te - Tw)) * dt + (1 + (stepQi * sigmalevel)) * exp(p22) * dw2)
## Set the names of the inputs
model$addInput(Te, Gv, Gbs1, Gbs2, Gbs3, Gbs4, Qi, stepQi)
##
## Set the observation equation: Ti is the state, yTi is the measured output
model$addObs(yTi ~ Ti)
## Set the variance of the measurement error
model$setVariance(yTi ~ exp(e11))
##
## Set the initial value (for the optimization) of the value of the state at the starting time point
model$setParameter(Ti = c(init = 35, lb = 10, ub = 45))
model$setParameter(Tw = c(init = 34, lb = 10, ub = 45))
## Set the initial value of the parameters for the optimization
model$setParameter(Ci = c(init = 1E5, lb = 1E4, ub = 1E7))
model$setParameter(Cw = c(init = 5E5, lb = 1E4, ub = 1E8))
model$setParameter(Riw = c(init = 0.1, lb = 1E-5, ub = 10))
model$setParameter(Rwe = c(init = 0.1, lb = 1E-5, ub = 10))
model$setParameter(gA1 = c(init = 0.2, lb = 0.000001, ub = 3))
model$setParameter(gA2 = c(init = 0.2, lb = 0.001, ub = 3))
model$setParameter(gA3 = c(init = 0.2, lb = 0.001, ub = 3))
model$setParameter(gA4 = c(init = 0.2, lb = 0.001, ub = 3))
model$setParameter(p11 = c(init = 1, lb = -50, ub = 10))
model$setParameter(p22 = c(init = 1, lb = -50, ub = 10))
model$setParameter(e11 = c(init = -1, lb = -50, ub = 10))
model$setParameter(sigmalevel = c(init = 1, lb = 0, ub = 100))
## Run the parameter optimization
fit <- model$estimate(data = datf, firstorder = pg$firstorder, threads = pg$threads)

## Check model
summary(fit, extended = TRUE)

fit$data[[1]] <- datf
fit$Rnames <- c("Riw", "Rwe")
analyzeFit(fit)

## Plot the estimated gA curve ----------------------------------------------------

## Keep the one-step predictions
val <- predict(fit)[[1]]

## Plot
xseq <- seq(1, 5, len = 100)
Xbs_seq <- bs(xseq, df = 4, Boundary.knots = c(1, 5), intercept = TRUE)
plot(xseq * 180 / (pi), Xbs_seq %*% fit$xm[c("gA1", "gA2", "gA3", "gA4")],
  ylab = "gA splined", xlab = expression("Sun Azimuth (" * degree * ")"), xlim = c(0, 360),
  type = "l", ylim = c(0, 0.12)
)
lines(xseq * 180 / (pi), Xbs_seq %*% (fit$xm[c("gA1", "gA2", "gA3", "gA4")] +
  1.96 * fit$sd[c("gA1", "gA2", "gA3", "gA4")]), lty = "dashed")
lines(xseq * 180 / (pi), Xbs_seq %*% (fit$xm[c("gA1", "gA2", "gA3", "gA4")] -
  1.96 * fit$sd[c("gA1", "gA2", "gA3", "gA4")]),
lty = "dashed"
)

