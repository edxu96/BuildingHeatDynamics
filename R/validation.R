# ./MatrixTSA/R/tool/validation.R: Functions for Model Validation
# Edward J. Xu, edxu96@outlook.com
# Aug 26, 2019

#' Model validation (check i.i.d. and distribution of residuals)
#' @param vec_resi vector of residuals
#' @param vec_in vector of input
plot_resi <- function(vec_resi, vec_in){
  par(mfrow = c(5, 1))
  ## Plot residuals
  plot(vec_resi)
  ## Residuals vs. input Te: There shouldn't be any relation
  ival <- as.integer(names(vec_resi))
  plot(vec_in, vec_resi)
  ## Check the distribution: Should not be too skewed
  hist(vec_resi)
  qqnorm(vec_resi)
  qqline(vec_resi)

  acf(vec_resi)
}


## Another great plot to check residuals
# df_x$residuals <- NA
# df_x$residuals[ival] <- fit$residuals
# pairs(df_x[c("residuals","t","Te")], panel = panel.smooth, lower.panel = NULL)
