# R Fucntion File for Global and Local Trend Model in Time Series Analysis
# Edward J. Xu
# Version: 1.0
# Date: March 17th, 2019
# ----------------------------------------------------------------------------------------------------------------------
# 1. Define functions for General Linear Model (GLM)
# ----------------------------------------------------------------------------------------------------------------------
#' Calculate the sum of squared error (sse)
#' 
#' @param mat_x matrix of realisation of independent variables
#' @param vec_y vector of observations from dependent variables
#' @param vec_theta.hat vector of estimated theta
#' @param mat_capSigma covariance matrix of residuals, default identity matrix
#' @return sum of squared error (sse)
#' @references P34 
cal_vec_sse.hat <- function(mat_x, vec_y, vec_theta.hat, mat_capSigma = diag(length(mat_x[,1]))){
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
cal_sigma.hat <- function(mat_x, vec_y, vec_theta.hat, mat_capSigma = diag(length(mat_x[,1]))){
    sigma.hat.square <- cal_vec_sse.hat(mat_x, vec_y, vec_theta.hat, mat_capSigma) /
        (length(mat_x[,1]) - length(vec_theta.hat))
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
cal_mat_var.theta.hat <- function(mat_x, sigma.hat, mat_capSigma= diag(length(mat_x[,1]))){
    mat_var.theta.hat <- sigma.hat^2 * solve(t(mat_x) %*% solve(mat_capSigma) %*% mat_x)
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
cal_mat_intervalConf <- function(prob = 0.95, mat_x, vec_theta.hat, vec_y){
    n <- length(mat_x[,1])
    p <- length(vec_theta.hat)
    quantileStudentDist <- qt(p = prob, df = n - p)
    vec_var.y.hat <- (vec_y - mat_x %*% vec_theta.hat)^2
    vec_boundUp <- mat_x %*% vec_theta.hat + quantileStudentDist * sqrt(vec_var.y.hat / n)
    vec_boundLow <- mat_x %*% vec_theta.hat - quantileStudentDist * sqrt(vec_var.y.hat / n)
    return(list(vec_boundUp = vec_boundUp, vec_boundLow = vec_boundLow))
}
#' Estimate theta using weighted least square
#' @param mat_x matrix of realisation of independent variables
#' @param vec_y vector of observations from dependent variables
#' @param mat_capSigma covariance matrix of residuals, default identity matrix
#' @return vector of estimated theta
#' @references eq 3.40, P38
pred_vec_theta.hat <- function(mat_x, vec_y, mat_capSigma = diag(length(mat_x[,1]))){
    vec_theta.hat <- solve(t(mat_x) %*% solve(mat_capSigma) %*% mat_x) %*% t(mat_x) %*% solve(mat_capSigma) %*% vec_y
    return(vec_theta.hat)
}
# ----------------------------------------------------------------------------------------------------------------------
# 2. Relaxation algorithm to estimate the covariance matrix of residuals with exponential decaying function
# ----------------------------------------------------------------------------------------------------------------------
#' Generate the covariance matrix of residuals with exponential decaying function
#' 
#' @param rho parameter in exponential decaying function
#' @param n size of the matrix of residuals
#' @return covariance matrix of residuals
#' @references NULL
do_mat_capSigma_expDecay <- function(rho, n){
    mat_capSigma <- diag(n)
    for (i in 1: n){
        for (j in 1: n){
            mat_capSigma[i, j] <- rho^(abs(i - j))
        }
    }
    return(mat_capSigma)
}
#' Generate the more prefered rho for exponential decaying function to generate next covariance matrix of residuals
#' 
#' @param mat_x matrix of realisation of independent variables
#' @param vec_epsilon vector of difference between estimated dependent variables and observations
#' @param n size of the matrix of residuals
#' @return more prefered rho
#' @references NULL
do_newRho <- function(mat_x, vec_epsilon, sigma, n){
    sum <- 0
    for (i in 1: (n - 1)){
        sum <- sum + vec_epsilon[i] * vec_epsilon[i+1]
    }
    rho <- sum / (sigma^2 * (n - 1))
    return(rho)
}
#' Relaxation algorithm to estimate the covariance matrix of residuals with exponential decaying function
#' 
#' @param mat_x matrix of realisation of independent variables
#' @param vec_y vector of observations from dependent variables
#' @return most prefered rho
#' @references P41
cal_rho_expDecayRelaxAlgo <- function(mat_x, vec_y){
    rho <- 0
    n <- length(mat_x[,1])
    for (t in 1: 5){
        mat_capSigma <- do_mat_capSigma_expDecay(rho, n)
        vec_theta.hat <- pred_vec_theta.hat(mat_x, vec_y, mat_capSigma)
        sigma.hat <- cal_sigma.hat(mat_x, vec_y, vec_theta.hat)
        vec_epsilon <- vec_y - mat_x %*% vec_theta.hat
        rho <- do_newRho(mat_x, vec_epsilon, sigma.hat, n)
    }
    return(rho)
}
# ----------------------------------------------------------------------------------------------------------------------
# 3. Define functions for trend model.
# ----------------------------------------------------------------------------------------------------------------------
#' Generate reversed sequence starting from zero and decreading by 1
#' 
#' @param n length of the sequence
#' @return the sequence
#' @references NULL
do_seq_zeroDecreaseRev <- function(n){return(rev(- seq(0, n-1)))}
#' Generate covariance matrix in trend model with forgetting factor (discount coefficient) lambda
#' 
#' @param lambda forgetting factor (discount coefficient)
#' @param n size of the matrix
#' @return covariance matrix
#' @references P56
do_mat_capSigma_trend <- function(lambda = 1, n){
    mat_capSigma_trend <- diag(n)
    seq_zdi <- do_seq_zeroDecreaseRev(n)
    for (i in 1: n){
        mat_capSigma_trend[i, i] <- 1 * lambda^seq_zdi[i]
    }
    return(mat_capSigma_trend)
}
cal_mat_capF_trend <- function(mat_x_trend, mat_capSigma_trend){
    mat_capF <- t(mat_x_trend) %*% solve(mat_capSigma_trend) %*% mat_x_trend
    return(mat_capF)
}
cal_vec_h_trend <- function(mat_x_trend, mat_capSigma_trend, vec_y_trend){
    mat_h <- t(mat_x_trend) %*% solve(mat_capSigma_trend) %*% vec_y_trend
    return(mat_h)
}
cal_vec_theta.hat_trend <- function(mat_x_trend, mat_capSigma_trend, vec_y_trend){
    mat_capF <- cal_mat_capF_trend(mat_x_trend, mat_capSigma_trend)
    mat_h <- cal_vec_h_trend(mat_x_trend, mat_capSigma_trend, vec_y_trend)
    vec_theta.hat_trend <- solve(mat_capF) %*% mat_h
    return(vec_theta.hat_trend)
}
cal_vec_intervalPred <- function(prob = 0.95, n, p, y.hat, var){
    quantileStudentDist <- qt(p = 0.95, df = n - p)
    boundUp <- y.hat + quantileStudentDist * sqrt(var)
    boundLow <- y.hat - quantileStudentDist * sqrt(var)
    return(list(boundUp = drop(boundUp), boundLow = drop(boundLow)))
}
cal_mat_intervalPred <- function(vec_l, prob = 0.95, n, p, vec_y.hat, vec_var){
    vec_boundUp <- numeric(length(vec_l))
    vec_boundLow <- numeric(length(vec_l))
    for (i in seq(1, length(vec_l))){
        interval <- cal_vec_intervalPred(prob, n, p, y.hat = vec_y.hat[i], var = vec_var[i])
        vec_boundUp[i] <- interval$boundUp
        vec_boundLow[i] <- interval$boundLow
    }
    return(list(vec_boundUp = vec_boundUp, vec_boundLow = vec_boundLow))
}
#' Calculate the vector of total memory in local trend model
cal_vec_memoryTotal <- function(lambda, n){
    vec_memoryTotal <- numeric(n)
    vec_memoryTotal[1] <- 1
    for (j in 1: (n-1)){
        vec_memoryTotal[j+1] <- vec_memoryTotal[j] + lambda^j
    }
    return(vec_memoryTotal)
}
# ----------------------------------------------------------------------------------------------------------------------
# 4. Define Functions for Linear Trend Model
# ----------------------------------------------------------------------------------------------------------------------
func_f_linear <- function(j){
    return(rbind(1, j))
}
pred_y_trend_linear <- function(l, vec_theta.hat_trend){
    y_pred_trend <- t(func_f_linear(l)) %*% vec_theta.hat_trend
    return(y_pred_trend)
}
pred_var_trend_linear <- function(l, mat_x_trend, vec_y_trend, vec_theta.hat_trend, mat_capSigma_trend){
    sigma.hat <- cal_sigma.hat(mat_x = mat_x_trend, vec_y = vec_y_trend, 
                               vec_theta.hat = vec_theta.hat_trend, mat_capSigma = mat_capSigma_trend)
    mat_capF_trend_trend <- cal_mat_capF_trend(mat_x_trend, mat_capSigma_trend)
    var_pred_trend <- sigma.hat^2 %*% (1 + t(func_f_linear(l)) %*% solve(mat_capF_trend_trend) %*% func_f_linear(l))
    return(var_pred_trend)
}
pred_vec_y_trend_linear <- function(vec_l, vec_theta.hat_trend){
    vec_y_trend_linear <- numeric(length(vec_l))
    for (i in (1: length(vec_l))){
        vec_y_trend_linear[i] <- pred_y_trend_linear(vec_l[i], vec_theta.hat_trend)
    }
    return(vec_y_trend_linear)
}
pred_vec_var_trend_linear <- function(vec_l, mat_x_trend, vec_y.pred_trend, vec_theta.hat_trend, mat_capSigma_trend){
    vec_var_trend_linear <- numeric(length(vec_l))
    for (i in (1: length(vec_l))){
        vec_var_trend_linear[i] <- pred_var_trend_linear(vec_l[i], mat_x_trend, 
                                                         vec_y.pred_trend[i], vec_theta.hat_trend, mat_capSigma_trend)
    }
    return(vec_var_trend_linear)
}
# ----------------------------------------------------------------------------------------------------------------------
# 5. Define Functions for Quadratic Local Trend Model
# ----------------------------------------------------------------------------------------------------------------------
func_f_quadratic <- function(j){
    return(rbind(1, j, j^2 / 2))
}
pred_y_trend_quadratic <- function(l, vec_theta.hat_trend){
    y_pred_trend <- t(func_f_quadratic(l)) %*% vec_theta.hat_trend
    return(y_pred_trend)
}
pred_vec_y_trend_quadratic <- function(vec_l, vec_theta.hat_trend){
    vec_y.pred_trend_quadratic <- numeric(length(vec_l))
    for (i in seq(1, length(vec_l))){
        vec_y.pred_trend_quadratic[i] <- pred_y_trend_quadratic(vec_l[i], vec_theta.hat_trend)
    }
    return(vec_y.pred_trend_quadratic)
}
pred_var_trend_quadratic <- function(l, mat_x_trend, vec_y_trend, vec_theta.hat_trend, mat_capSigma_trend){
    sigma.hat <- cal_sigma.hat(mat_x = mat_x_trend, vec_y = vec_y_trend, 
                               vec_theta.hat = vec_theta.hat_trend, mat_capSigma = mat_capSigma_trend)
    mat_capF_trend_trend <- cal_mat_capF_trend(mat_x_trend, mat_capSigma_trend)
    var.pred_trend <- sigma.hat^2 %*% (1 + t(func_f_quadratic(l)) %*% solve(mat_capF_trend_trend) %*% func_f_quadratic(l))
    return(var.pred_trend)
}
pred_vec_var_trend_quadratic <- function(vec_l, mat_x_trend, vec_y.pred_trend, vec_theta.hat_trend, mat_capSigma_trend){
    vec_var.pred_trend_quadratic <- numeric(length(vec_l))
    for (i in seq(1, length(vec_l))){
        vec_var.pred_trend_quadratic[i] <- pred_var_trend_quadratic(vec_l[i], 
                                                                    mat_x_trend, 
                                                                    vec_y.pred_trend[i], 
                                                                    vec_theta.hat_trend, 
                                                                    mat_capSigma_trend)
    }
    return(vec_var.pred_trend_quadratic)
}
# ----------------------------------------------------------------------------------------------------------------------