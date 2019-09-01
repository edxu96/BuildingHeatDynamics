# Grey-Box Modelling of Heat Dynamics of a Building using SDEs
# Edward J. Xu, edxu96@outlook.com
# Aug 17, 2019

rm(list = ls())

library(ctsmr)

files <- dir(
  "~/GitHub/MatrixTSA/examples_source/greybox/src/funcs",
  full.names = TRUE
  )
for (i in 1:length(files)) {
  source(files[i])
}
source("~/GitHub/MatrixTSA/examples_source/greybox/src/func.R")

datf <- get_datf()
plot_datf(datf)
mod <- set_mod()

mod_est <- mod$estimate(datf)
# Note that for the estimation the following could be set (the default values
# fits the current case): firstorderinputinterpolation = FALSE, means zero
# order hold of the inputs between the sample points threads = 1, the
# optimization can use multiple threads (can create some crash issues)

summary(mod_est)

summary(mod_est, extended = TRUE)
# If any of the parameter estimates are close to the lower or upper bound then
# "dF/dPar" is significant compered to "dPen/dPar"

## Calculate the one-step predictions of the state (i.e. the residuals)
li_pred <- predict(mod_est)[[1]]
str(li_pred)

## Calculate the residuals and put them with the data in a data.frame datf
datf$residuals <- datf$yTi - li_pred$output$pred$yTi

plot_resi(datf$residuals)

plot_resi_2(datf)
# datf[per(tstart, datf$timedate,tstart+2*24*3600),]

