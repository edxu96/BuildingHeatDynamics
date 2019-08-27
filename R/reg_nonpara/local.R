

#' Triangular Kernel Function
#'   x_i: the point for which to center
#'   x: the data points
#'   h: the bandwidth
#'   plot(x, tri(x_i = 0, x = x, h = 0.4))
weight_tri <- function(x_i, x, h){
  val <- 1 - abs((x - x_i) / h)
  val[val < 0] <- 0
  return(val)
}

#' Epanechnikov kernel function
weight_epanechnikov <- function(x_i, x, h){
  ## Epanechnikov kernel
  u <- abs(x - x_i)
  u <- u / h
  val <- 3 / 4 * (1 - u^2)
  ## Set values with |u|>1 to 0
  val[abs(u) > 1] <- 0
  return(val)
}

optim_reg_local <- function(bandwidth, datf_x){
  result <- reg_local(bandwidth, datf_x)
  return(result$rmse_mean)
}
