
plot_bs <- function(x, x_bs){
    ## Merge them with x
    df_x <- data.frame(x, x_bs)
    names(df_x) <- c("x", paste0("bs", 1: (ncol(df_x) - 1)))
    ## Plot
    icol <- grep("^bs", names(df_x))
    plot(df_x$x, df_x$bs1, type="n", ylim=range(df_x[ ,icol]))
    for(i in icol){
        lines(df_x$x, df_x[ ,i], col = i)
    }
}

#' Regression using Base Splines
reg_bs <- function(datf, df){
  lm <- lm(P ~ bs(Te, df = df), datf)

  seq_x <- seq(min(df_x$Te), max(df_x$Te), len = 100)
  seq_y <- predict(fit, newdata = data.frame(Te = seq_x))
  plot(df_x$Te, df_x$P)
  ## We need to use predict to see the estimated function
  lines(seq_x, seq_y)
  return(lm)
}

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
