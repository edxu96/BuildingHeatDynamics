# Linear Trend Model


func_f_linear <- function(j) {
  return(rbind(1, j))
}

pred_y_trend_linear <- function(l, vec_theta.hat_trend) {
  y_pred_trend <- t(func_f_linear(l)) %*% vec_theta.hat_trend
  return(y_pred_trend)
}

pred_var_trend_linear <- function(
    l, mat_x_trend, vec_y_trend, vec_theta.hat_trend, mat_capSigma_trend
    ) {
  sigma.hat <- cal_sigma.hat(
    mat_x = mat_x_trend, vec_y = vec_y_trend,
    vec_theta.hat = vec_theta.hat_trend, mat_capSigma = mat_capSigma_trend
  )
  mat_capF_trend_trend <- cal_mat_capF_trend(mat_x_trend, mat_capSigma_trend)
  var_pred_trend <- sigma.hat^2 %*% (
    1 + t(func_f_linear(l)) %*% solve(mat_capF_trend_trend) %*%
      func_f_linear(l)
    )
  return(var_pred_trend)
}

pred_vec_y_trend_linear <- function(vec_l, vec_theta.hat_trend) {
  vec_y_trend_linear <- numeric(length(vec_l))
  for (i in (1:length(vec_l))) {
    vec_y_trend_linear[i] <- pred_y_trend_linear(vec_l[i], vec_theta.hat_trend)
  }
  return(vec_y_trend_linear)
}

pred_vec_var_trend_linear <- function(
    vec_l, mat_x_trend, vec_y.pred_trend, vec_theta.hat_trend,
    mat_capSigma_trend
    ) {
  vec_var_trend_linear <- numeric(length(vec_l))
  for (i in (1:length(vec_l))) {
    vec_var_trend_linear[i] <- pred_var_trend_linear(
      vec_l[i], mat_x_trend,
      vec_y.pred_trend[i], vec_theta.hat_trend, mat_capSigma_trend
    )
  }
  return(vec_var_trend_linear)
}
