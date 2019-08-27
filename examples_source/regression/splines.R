# Functions for Splines Regression for Nonlinear TSA
# Edward J. Xu

library(splines)

#' Generate the matrix of base splines for uni-variate input
#' @param vec_x Input time series
#' @param df Degree of freedom in base splines function
get_mat_x_bs <- function(vec_x, df, whe_knots = TRUE, whe_plot = FALSE){
  if (whe_knots) {
    knots <- quantile(vec_x, probs = seq(0, 1, by = 1 / (df - 1)))
    mat_x_bs <- bs(
      vec_x, knots = knots[2:(length(knots) - 1)], degree = 3, intercept = TRUE
      )[ , ]
  } else {
    mat_x_bs <- bs(vec_x, df = 7, intercept = TRUE)[ , ]
  }
  if (whe_plot){
    plot_bs(vec_x, mat_x_bs)
    if (whe_knots) {
      abline(v = knots, col = 2)
    }
  }
  return(mat_x_bs)
}

#' Fit the best lm model using base splines method
#' @param li List of output and input
get_lm_bs <- function(li){


  get_mat_x_bs(li$x, df, whe_knots = TRUE, whe_plot = FALSE)
  lm_bs <- lm(y ~ x, data = li)
  return(lm_bs)
}
