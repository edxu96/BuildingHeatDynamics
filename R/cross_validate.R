# Functions for cross validation
# Edward J. Xu
# Aug 26, 2019

#' Root Mean Squared Error RMSE
#'   Because we don't count the NA value in the following calculation,
#'   the length of the vec_pred must minus the NA prediction.
#'   Usually, there is no NA in prediction.
cal_rmse <- function(vec_pred, vec_valid){
  rmse <- sqrt(mean((vec_valid - vec_pred)^2, na.rm = TRUE))
  return(rmse)
}

reg_local <- function(bandwidth, num_fold, datf){
  vec_pred <- rep(NA, nrow(datf))
  vec_rmse <- rep(NA, nrow(datf))
  for (i in 1:nrow(datf)) {
    vec_sub_i <- (1:nrow(datf))[-i]
    # The i'th observation will not be included in the fit
    lm <- lm(
      y ~ x, data = datf, subset = vec_sub_i,
      weight = weight_tri(datf$x[i], datf$x, h = bandwidth)
      )
    vec_pred[i] <- predict(lm, newdata = data.frame(x = datf$x[i]))
    vec_rmse[i] <- cal_rmse(vec_pred = vec_pred[i], vec_valid = datf$y[i])
  }
  rmse_mean <- mean(vec_rmse, na.rm = TRUE)

  # plot(datf$x, datf$y)
  # lines(x, vec_pred, col = 2)

  return(list(
    "vec_pred" = vec_pred, "rmse_mean" = rmse_mean
    ))
}

#' K-Fold Cross Validation
crossvalid <- function(datf, num_fold = 10){
    vec_rmse <- rep(NA, num_fold)
    for (i in 1:numFold) {
        datForVali <- datf[(datf$index == i),]
        vec_pred <- predict(lm, newdata = data.frame(x = datf$x[i]))
        vecRootMeanSquaredError[i] <- cal_rmse <- function(vec_pred, vec_valid)
    }
    aveRootMeanSquaredError <- sum(vecRootMeanSquaredError) / num_fold
    return(aveRootMeanSquaredError)
}
