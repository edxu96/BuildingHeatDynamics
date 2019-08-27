# General Linear Model (GLM)
# Edward J. Xu
# Aug 26th, 2019

#' Calculate the sum of squared error (sse)
#'
#' @param mat_x matrix of realisation of independent variables
#' @param vec_y vector of observations from dependent variables
#' @param vec_theta.hat vector of estimated theta
#' @param mat_capSigma covariance matrix of residuals, default identity matrix
#' @return sum of squared error (sse)
#' @references P34
cal_vec_sse.hat <- function(
    mat_x, vec_y, vec_theta.hat, mat_capSigma = diag(length(mat_x[, 1]))
    ) {
  vec_epsilon <- vec_y - mat_x %*% vec_theta.hat
  sse.hat <- t(vec_epsilon) %*% solve(mat_capSigma) %*% vec_epsilon
  return(sse.hat)
}

#' Estimate the sigma
#'
#' @param mat_x matrix of realisation of independent variables
#' @param vec_y vector of observations from dependent variables
#' @param vec_theta.hat vector of estimated theta
#' @param mat_capSigma covariance matrix of residuals, default identity matrix
#' @return estimated sigma
#' @references eq 3.44, Theorem 3.4, P39
cal_sigma.hat <- function(
    mat_x, vec_y, vec_theta.hat, mat_capSigma = diag(length(mat_x[, 1]))
    ) {
  sigma.hat.square <- cal_vec_sse.hat(
    mat_x, vec_y, vec_theta.hat, mat_capSigma
    ) / (length(mat_x[, 1]) - length(vec_theta.hat))
  sigma.hat <- sqrt(drop(sigma.hat.square))
  return(sigma.hat)
}

#' Calculate the variance matrix of estimated theta
#'
#' @param mat_x matrix of realisation of independent variables
#' @param sigma.hat estimated sigma
#' @param mat_capSigma covariance matrix of residuals, default identity matrix
#' @return variance matrix estimated theta
#' @references eq 3.43, P39
cal_mat_var.theta.hat <- function(
    mat_x, sigma.hat, mat_capSigma = diag(length(mat_x[, 1]))
    ) {
  mat_var.theta.hat <- sigma.hat^2 *
    solve(t(mat_x) %*% solve(mat_capSigma) %*% mat_x)
  return(drop(mat_var.theta.hat))
}

#' Calculate the matrix of confidence interval ???
#'
#' @param prob probability of the confidence interval, default 0.95
#' @param mat_x matrix of realisation of independent variables
#' @param vec_theta.hat vector of estimated theta
#' @param vec_y vector of observations from dependent variables
#' @return list of vectors of upper bound and lower bound
#' @references NULL
cal_mat_intervalConf <- function(prob = 0.95, mat_x, vec_theta.hat, vec_y) {
  n <- length(mat_x[, 1])
  p <- length(vec_theta.hat)
  quantileStudentDist <- qt(p = prob, df = n - p)
  vec_var.y.hat <- (vec_y - mat_x %*% vec_theta.hat)^2
  vec_boundUp <- mat_x %*% vec_theta.hat + quantileStudentDist *
    sqrt(vec_var.y.hat / n)
  vec_boundLow <- mat_x %*% vec_theta.hat - quantileStudentDist *
    sqrt(vec_var.y.hat / n)
  return(list(vec_boundUp = vec_boundUp, vec_boundLow = vec_boundLow))
}

#' Estimate theta using weighted least square
#' @param mat_x matrix of realisation of independent variables
#' @param vec_y vector of observations from dependent variables
#' @param mat_capSigma covariance matrix of residuals, default identity matrix
#' @return vector of estimated theta
#' @references eq 3.40, P38
pred_vec_theta.hat <- function(
    mat_x, vec_y, mat_capSigma = diag(length(mat_x[, 1]))
    ) {
  vec_theta.hat <- solve(t(mat_x) %*% solve(mat_capSigma) %*% mat_x) %*%
    t(mat_x) %*% solve(mat_capSigma) %*% vec_y
  return(vec_theta.hat)
}
