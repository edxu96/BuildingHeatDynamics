## Functions for Model Predictive Control
## Edward J. Xu
## Aug 31, 2019

#' Transform the CTSM model to state space model
#' @param mod_ctsm CTMS model to be tranformed
#' @export
trans_ctsm_ss <- function(mod_ctsm){
  Estimated <- list2env(as.list(mod_ctsm$xm))

  li_mat_ss = list(
    a = matrix(
      sapply(mod_ctsm$model$sys.eqs$amat, eval, envir = Estimated),
      nrow = length(mod_ctsm$model$states)
      ),
    b = matrix(
      sapply(mod_ctsm$model$sys.eqs$bmat, eval, envir = Estimated),
      nrow = length(mod_ctsm$model$states)
      ),
    c = matrix(
      sapply(mod_ctsm$model$obs.eqs$cmat, eval, envir = Estimated),
      nrow = length(mod_ctsm$model$outputs)
      ),
    D = matrix(
      sapply(mod_ctsm$model$obs.eqs$dmat, eval, envir = Estimated),
      nrow = length(mod_ctsm$model$outputs)
      )
    )

  return(li_mat_ss)
}

#' Transform the matrix for continuous state space model to matrix for
#' discerete state space model.
#' @param li_mat_ss List of matrix for continuous state space model
#' @param ti_data Tibble of data for estimation
#' @import expm expm
#' @export
trans_mat_ss <- function(li_mat_ss, ti_data){
  dt <- diff(ti_data$t)[1] # all.equal(diff(diff(ti$t)),rep(0,length(ti$t)-2))
  ## Make sure that time steps are uniform

  a <- expm(li_mat_ss$a * dt)
  b <- solve(li_mat_ss$a) %*% (a - diag(dim(li_mat_ss$a)[1])) %*% li_mat_ss$b

  li_mat_ss_d = list(
    a = a,
    b = b,
    c = li_mat_ss$c,
    D = li_mat_ss$d
  )

  return(li_mat_ss_d)
}



