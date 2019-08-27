# Quadratic Local Trend Model
# Edward J. Xu
# Aug 26th, 2019

func_f_quadratic <- function(j) {
  return(rbind(1, j, j^2 / 2))
}

pred_y_trend_quadratic <- function(l, vec_theta.hat_trend) {
  y_pred_trend <- t(func_f_quadratic(l)) %*% vec_theta.hat_trend
  return(y_pred_trend)
}

pred_vec_y_trend_quadratic <- function(vec_l, vec_theta.hat_trend) {
  vec_y.pred_trend_quadratic <- numeric(length(vec_l))
  for (i in seq(1, length(vec_l))) {
    vec_y.pred_trend_quadratic[i] <- pred_y_trend_quadratic(
      vec_l[i], vec_theta.hat_trend
      )
  }
  return(vec_y.pred_trend_quadratic)
}

pred_var_trend_quadratic <- function(
    l, mat_x_trend, vec_y_trend, vec_theta.hat_trend, mat_capSigma_trend
    ) {
  sigma.hat <- cal_sigma.hat(
    mat_x = mat_x_trend, vec_y = vec_y_trend,
    vec_theta.hat = vec_theta.hat_trend, mat_capSigma = mat_capSigma_trend
  )
  mat_capF_trend_trend <- cal_mat_capF_trend(mat_x_trend, mat_capSigma_trend)
  var.pred_trend <- sigma.hat^2 %*% (
    1 + t(func_f_quadratic(l)) %*% solve(mat_capF_trend_trend) %*%
      func_f_quadratic(l)
    )
  return(var.pred_trend)
}

pred_vec_var_trend_quadratic <- function(
    vec_l, mat_x_trend, vec_y.pred_trend, vec_theta.hat_trend,
    mat_capSigma_trend
    ) {
  vec_var.pred_trend_quadratic <- numeric(length(vec_l))
  for (i in seq(1, length(vec_l))) {
    vec_var.pred_trend_quadratic[i] <- pred_var_trend_quadratic(
      vec_l[i],
      mat_x_trend,
      vec_y.pred_trend[i],
      vec_theta.hat_trend,
      mat_capSigma_trend
    )
  }
  return(vec_var.pred_trend_quadratic)
}
