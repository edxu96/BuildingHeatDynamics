## Function to Set CTSM Model
## Edward J. Xu, edxu96@outlook.com
## Aug 30, 2019

#' Convert the `d(x)` to `dx``
d <- function(x) {
  return(
    list(
      bool = TRUE,
      chr_new = paste("d", deparse(substitute(x)), sep = ""),
      chr_old = paste("d(", deparse(substitute(x)), ")", sep = "")
    )
  )
}

#' Convert the standard expression to that for CTSM
convert_expr <- function(expr) {
  li_expr <- attributes(terms(expr))
  chr_expr <- stri_paste(
    as.character(expr)[2], " ",
    as.character(expr)[1], " ",
    as.character(expr)[3],
    collapse = " "
  )

  for (i in 2:length(li_expr$variables)) {
    whe_call <- is.call(li_expr$variables[[i]])
    if (whe_call) {

      options(show.error.messages = F)
      whe_d <- try(eval(li_expr$variables[[i]])$bool)
      if (!((class(whe_d) == "try-error") || (is.null(whe_d)))) {
        chr_old <- eval(li_expr$variables[[i]])$chr_old
        chr_new <- eval(li_expr$variables[[i]])$chr_new

        li_chr_expr = c(strsplit(chr_expr, split = " ")[[1]])

        li_chr_expr[li_chr_expr == chr_old] <- chr_new

        chr_expr <- stri_paste(li_chr_expr, collapse = " ")
      } else {
        next
      }
    }
  }
  options(show.error.messages = T)
  expr_new <- as.formula(chr_expr)
  return(expr_new)
}

#' Set a new CTSM model
#' @param c_expr_sys Vector of system expressions
#' @param expr_obs Observation expressions
#' @param expr_var Variance Expressions
#' @param c_input Name of Input Variables
#' @param datf_para Dataframe of estimated parameters
#' @value New CTSM model
#' @export
set_mod_ctsm <- function(c_expr_sys, expr_obs, expr_var, c_input, ti_est) {
  ## Initialize a CTSM model
  mod <- ctsm$new()

  ## Add formulas
  lapply(lapply(c_expr_sys, convert_expr), mod$addSystem)

  mod$addObs(expr_obs)
  mod$setVariance(expr_var)

  ## Add input variables
  for (i in 1:length(c_input)) {
    eval(parse(text = paste(
      "mod$addInput(", c_input, ")", sep = ""
      )))
  }

  ## Add initial values and bounds for parameters estimation
  for (i in 1:nrow(ti_est)) {
    eval(parse(text = paste(
      "mod$setParameter(",
      ti_est$name[i], " = c(init = ", as.character(ti_est$init[i]),
      ", lb = ", as.character(ti_est$lb[i]),
      ", ub = ", as.character(ti_est$up[i]), "))",
      sep = ""
      )))
  }

  return(mod)
}

#' Estimate the model using data
#' @param mod CTMS Model
#' @param ti_data Tibble of data
#' @export
est_mod_ctsm <- function(mod, ti_data){
  return(mod$estimate(as.data.frame(ti_data)))
}







