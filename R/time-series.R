## Functions for Time Series
## Edward J. Xu
## Sept 7, 2019

#### 1, Multiple Series ####
## Functions for sets of time series with same time point.
## For example, four wind speed values every time point
## It's hard to plot the data when they are spreaded in different columns using
##   ggplot2. It's better to put them in the same column called "speed" with
##   other indexed indicating which kind of wind speed it is.

#' Combine the data together
stretch <- function(ts){
  return(ts_stretched)
}
