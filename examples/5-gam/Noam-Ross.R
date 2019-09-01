
library(mgcr)

## 1, Univariate GAMS for mcycle ----
mcycle <- MASS::mcycle

plot(mcycle$accel, mcycle$times)

# Fit the model
gam_mod <- gam(
  times ~ s(accel, k = 30), data = mcycle, sp = 0.01, method = "REML"
  )

plot(gam_mod, residuals = TRUE, pch = 1)

summary(gam_mod)

## 2, Multivariate GAMs for Auto Performance ----

library(gamair)
data("mpg", package = "gamair")

mod_city <- gam(
  city.mpg ~ s(weight) + s(length) + s(price), data = mpg, method = "REML"
  )

plot(mod_city, pages = 1, bty = "n")
