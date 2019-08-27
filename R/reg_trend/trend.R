# Global and Local Trend Model in Time Series Analysis
# Edward J. Xu
# Aug 26th, 2019

# Linear trend model.

#' Generate reversed sequence starting from zero and decreading by 1
#'
#' @param n length of the sequence
#' @return the sequence
#' @references NULL
do_seq_zeroDecreaseRev <- function(n) {
  return(rev(-seq(0, n - 1)))
}

#' Generate covariance matrix in trend model with forgetting factor
#'     (discount coefficient) lambda
#'
#' @param lambda forgetting factor (discount coefficient)
#' @param n size of the matrix
#' @return covariance matrix
#' @references P56
do_mat_capSigma_trend <- function(lambda = 1, n) {
  mat_capSigma_trend <- diag(n)
  seq_zdi <- do_seq_zeroDecreaseRev(n)
  for (i in 1:n) {
    mat_capSigma_trend[i, i] <- 1 * lambda^seq_zdi[i]
  }
  return(mat_capSigma_trend)
}
cal_mat_capF_trend <- function(mat_x_trend, mat_capSigma_trend) {
  mat_capF <- t(mat_x_trend) %*% solve(mat_capSigma_trend) %*% mat_x_trend
  return(mat_capF)
}
cal_vec_h_trend <- function(mat_x_trend, mat_capSigma_trend, vec_y_trend) {
  mat_h <- t(mat_x_trend) %*% solve(mat_capSigma_trend) %*% vec_y_trend
  return(mat_h)
}
cal_vec_theta.hat_trend <- function(
  mat_x_trend, mat_capSigma_trend, vec_y_trend
) {
  mat_capF <- cal_mat_capF_trend(mat_x_trend, mat_capSigma_trend)
  mat_h <- cal_vec_h_trend(mat_x_trend, mat_capSigma_trend, vec_y_trend)
  vec_theta.hat_trend <- solve(mat_capF) %*% mat_h
  return(vec_theta.hat_trend)
}
cal_vec_intervalPred <- function(prob = 0.95, n, p, y.hat, var) {
  quantileStudentDist <- qt(p = 0.95, df = n - p)
  boundUp <- y.hat + quantileStudentDist * sqrt(var)
  boundLow <- y.hat - quantileStudentDist * sqrt(var)
  return(list(boundUp = drop(boundUp), boundLow = drop(boundLow)))
}
cal_mat_intervalPred <- function(
  vec_l, prob = 0.95, n, p, vec_y.hat, vec_var
) {
  vec_boundUp <- numeric(length(vec_l))
  vec_boundLow <- numeric(length(vec_l))
  for (i in seq(1, length(vec_l))) {
    interval <- cal_vec_intervalPred(
      prob, n, p, y.hat = vec_y.hat[i], var = vec_var[i]
    )
    vec_boundUp[i] <- interval$boundUp
    vec_boundLow[i] <- interval$boundLow
  }
  return(list(vec_boundUp = vec_boundUp, vec_boundLow = vec_boundLow))
}
#' Calculate the vector of total memory in local trend model
cal_vec_memoryTotal <- function(lambda, n) {
  vec_memoryTotal <- numeric(n)
  vec_memoryTotal[1] <- 1
  for (j in 1:(n - 1)) {
    vec_memoryTotal[j + 1] <- vec_memoryTotal[j] + lambda^j
  }
  return(vec_memoryTotal)
}
