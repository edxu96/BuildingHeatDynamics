# Edward J. Xu

#' Get daraframe of data from building dynamics
get_datf <- function(){
  datf <- read.csv(
    "~/GitHub/MatrixTSA/data/inputPRBS1.csv", sep = ";", header = TRUE
    )
  datf$timedate <- asP("2009-02-05 14:26:00)") + datf$t * 3600
  return(datf)
}

#' Plot the time series from datf
#' @param datf Daraframe of data from building dynamics
plot_datf <- function(datf){
  ##
  setpar("ts", mfrow = c(4,1))
  gridSeq <- seq(asP("2009-01-01"),by="days",len=365)
  ##
  plot(datf$timedate, datf$yTi, type="n",ylab="yTi")
  abline(v = gridSeq, h = 0, col = "grey92")
  lines(datf$timedate,datf$yTi)
  ##
  plot(datf$timedate, datf$Ta, type="n",ylab="Ta")
  abline(v=gridSeq,h=0,col="grey92")
  lines(datf$timedate,datf$Ta)
  ##
  plot(datf$timedate, datf$Ph, type="n",ylab="Ph")
  abline(v=gridSeq,h=0,col="grey92")
  lines(datf$timedate,datf$Ph)
  ##
  plot(datf$timedate, datf$Ps, type="n",ylab="Ps")
  abline(v=gridSeq,h=0,col="grey92")
  lines(datf$timedate,datf$Ps)
  ##
  plotTSXAxis(datf$timedate, format="%Y-%m-%d")
}

#' Set a new CTSM model
#' @value model New CTSM model
set_mod <- function(){
  model <- ctsm$new()

  model$addSystem(
    dTi ~ ( 1 / (Ci * Ria) * (Ta - Ti) + gA / Ci * Ps + 1 / Ci * Ph ) * dt +
      exp(p11) * dw1
  )

  ## Note that the deterministic part of the SDE is multiplied with dt.
  ## Note that the stochastic part is multiplied with system noise process dw1
  ## Note that the variance of the system noise is exp(p11), where exp() is the
  ##   exponential function and p11 is the parameter. Since the variance is
  ##   strictly positive, but can be very close to zero, it is a good idea to
  ##   take exp() of the parameter, since then p11 can go from -Inf to Inf but
  ##   the exponential goes from 0 to Inf, with good resolution towards 0.

  ## Set the names of the inputs (simply the same as in the data.frame used
  ##   for estimation below)
  model$addInput(Ta, Ps, Ph)

  ## Set the observation equation: Ti is the state, yTi is the measured output
  model$addObs(yTi ~ Ti)

  ## Set the variance of the measurement error
  model$setVariance(yTi ~ exp(e11))

  ## Set the initial value of the value of the state at the start time (values
  ## where the estimation (i.e. optimization of the likelihood) starts) and
  ## also the lower and upper bound, which must contain the parameter value
  model$setParameter(  Ti = c(init = 15, lb = 0, ub = 25 ) )

  ## Set the initial values and bounds for the optimization of the parameters
  model$setParameter(  Ci = c(init = 1, lb = 1E-5, ub = 20 ) )
  model$setParameter( Ria = c(init = 20, lb = 10, ub = 1E4) )
  model$setParameter(  gA = c(init = 20, lb = 1, ub = 300) )
  model$setParameter( p11 = c(init = 1, lb = -30, ub = 10 ) )
  model$setParameter( e11 = c(init = -1, lb = -50,ub = 10 ) )

  return(model)
}

#' Plot the analyzing plots for residuals
#' @param vec_resi Vector of residuals
plot_resi <- function(vec_resi){
  ## Plot the auto-correlation function and cumulated periodogram in a new window
  par(mfrow=c(3, 1))
  ## The blue lines indicates the 95% confidence interval, meaning that if it is
  ##  white noise, then approximately 19 out of 20 lag correlations will be inside.
  acf(vec_resi, lag.max = 8 * 24)
  ## The periodogram is the estimated energy spectrum in the signal
  spec.pgram(vec_resi)
  ## The cumulated periodogram
  cpgram(vec_resi)
}

#' Plot the analyzing plots for residuals
#' @param datf Dataframe of data from building dynamics with prediction residuals
plot_resi_2 <- function(datf){
  ## Plot the residuals
  ## Prepare a time series plot (see "functions/setpar.R")
  setpar("ts", mfrow = c(5, 1))
  gridSeq <- seq(asP("2009-01-01"), by = "days", len = 365)
  ##
  plot(datf$timedate,datf$residuals,type="n",ylab="residuals")
  abline(v=gridSeq,h=0,col="grey92")
  lines(datf$timedate,datf$residuals)
  title(main=as.character(match.call())[2],line=-2,cex.main=2)
  ##
  plot(datf$timedate,datf$Ph,type="n",ylab="Ph")
  abline(v=gridSeq,h=0,col="grey92")
  lines(datf$timedate,datf$Ph)
  ##
  plot(datf$timedate,datf$yTi,type="n",ylab="yTi")
  abline(v=gridSeq,h=0,col="grey92")
  lines(datf$timedate,datf$yTi)
  lines(datf$timedate,datf$yTi-datf$residuals,col=2)
  legend("bottomright",c("Measured","Predicted"),lty=1,col=1:2,bg="grey95")
  ##
  plot(datf$timedate,datf$Ta,type="n",ylab="Ta")
  abline(v=gridSeq,h=0,col="grey92")
  lines(datf$timedate,datf$Ta)
  ##
  plot(datf$timedate,datf$Ps,type="n",ylab="Ps")
  abline(v=gridSeq,h=0,col="grey92")
  lines(datf$timedate,datf$Ps)
  ##
  plotTSXAxis(datf$timedate,format="%Y-%m-%d")
}

get_datf_2 <- function(){
  datf <- read.csv(
    "~/GitHub/MatrixTSA/data/inputPRBS1.csv", sep = ";", header = TRUE
    )
  datf$timedate <- asP("2009-02-05 14:26:00)") + datf$t * 3600
  return(datf)
}

get_pg <- function(){
  ## A list of parameters used in many functions
  pg <- list()
  ## Return the models or read from cache?
  pg$rerun <- FALSE
  pg$cache_dir <- "cache_fits"
  ## Use first order interpolation when estimating?
  pg$firstorder <- TRUE
  ## Number of threads used, keep at 1
  pg$threads <- 1
  ## Latitude and longitude of the position
  pg$latitude <- 37.097083
  pg$longitude <- -2.364994
  return(pg)
}
