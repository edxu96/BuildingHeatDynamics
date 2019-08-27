# Relaxation algorithm to estimate the covariance matrix of residuals with
#     exponential decaying function
# Edward J. Xu
# Aug 26th, 2019

#' Generate the covariance matrix of residuals with exponential decaying
#'     function
#'
#' @param rho parameter in exponential decaying function
#' @param n size of the matrix of residuals
#' @return covariance matrix of residuals
#' @references NULL
do_mat_capSigma_expDecay <- function(rho, n) {
  mat_capSigma <- diag(n)
  for (i in 1:n) {
    for (j in 1:n) {
      mat_capSigma[i, j] <- rho^(abs(i - j))
    }
  }
  return(mat_capSigma)
}

#' Generate the more prefered rho for exponential decaying function to generate
#'     next covariance matrix of residuals
#'
#' @param mat_x matrix of realisation of independent variables
#' @param vec_epsilon vector of difference between estimated dependent
#'     variables and observations
#' @param n size of the matrix of residuals
#' @return more prefered rho
#' @references NULL
do_newRho <- function(mat_x, vec_epsilon, sigma, n) {
  sum <- 0
  for (i in 1:(n - 1)) {
    sum <- sum + vec_epsilon[i] * vec_epsilon[i + 1]
  }
  rho <- sum / (sigma^2 * (n - 1))
  return(rho)
}

#' Relaxation algorithm to estimate the covariance matrix of residuals with
#'     exponential decaying function
#'
#' @param mat_x matrix of realisation of independent variables
#' @param vec_y vector of observations from dependent variables
#' @return most prefered rho
#' @references P41
cal_rho_expDecayRelaxAlgo <- function(mat_x, vec_y) {
  rho <- 0
  n <- length(mat_x[, 1])
  for (t in 1:5) {
    mat_capSigma <- do_mat_capSigma_expDecay(rho, n)
    vec_theta.hat <- pred_vec_theta.hat(mat_x, vec_y, mat_capSigma)
    sigma.hat <- cal_sigma.hat(mat_x, vec_y, vec_theta.hat)
    vec_epsilon <- vec_y - mat_x %*% vec_theta.hat
    rho <- do_newRho(mat_x, vec_epsilon, sigma.hat, n)
  }
  return(rho)
}
