
library(ctsmr)

#' Set a new CTSM model
#' @param mat_bound Matrix of initial values and bounds for optimization
#' @param eval_input Evaluation to add input to the model
#' @value New CTSM model
#' @example
#' set_mod_ctsm(
#'   f_state = dTi ~
#'     (1 / (Ci * Ria) * (Ta - Ti) + gA / Ci * Ps + 1 / Ci * Ph) * dt +
#'     exp(p11) * dw1,
#'   f_obs = yTi ~ Ti,
#'   f_error = yTi ~ exp(e11),
#'
#'
#'   )
#'
#' @export
set_mod_ctsm_formula <- function(f_state, f_obs, f_error) {

  eval_input <- eval(mod$addInput(Gh, Qi, stepQi, Te))

  model <- ctsm$new()
  model$addSystem(f_state)
  model$addObs(f_obs)
  model$setVariance(f_error)
  eval_input
  return(model)
}

mod <- set_mod_ctsm_formula(
)

mod$setParameter(Ti = c(init = 15, lb = 0, ub = 25))
mod$setParameter(Ci = c(init = 1, lb = 1E-5, ub = 20))
mod$setParameter(Ria = c(init = 20, lb = 10, ub = 1E4))
mod$setParameter(gA = c(init = 20, lb = 1, ub = 300))
mod$setParameter(p11 = c(init = 1, lb = -30, ub = 10))
mod$setParameter(e11 = c(init = -1, lb = -50, ub = 10))


