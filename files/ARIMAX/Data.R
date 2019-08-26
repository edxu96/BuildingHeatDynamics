## Data for dtu02417a3
## Edward J. Xu
## April 25th, 2019
frameData <- function(){
    dat <- read.csv("house_data_30min.csv")
    time <- as.numeric(dat[ , 1])
    heating <- as.numeric(dat[ , 2])
    tempExternal <- as.numeric(dat[ , 3])
    iSolar <- as.numeric(dat[ , 4])
    series <- seq(1, length(time))
    dat.f <- data.frame(series, time, heating, tempExternal, iSolar)
    return(dat.f)
}
dat.f <- frameData()
rm(frameData)
num_obs <- length(dat.f$series) - 4

