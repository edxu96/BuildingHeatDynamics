
#' Test `convert_expr()``
test_convert_expr <- function(){
  expr <- d(Ti) ~ 1 / Ci * Ph * d(t) + exp(p11) * d(w1)
  expr_new <- convert_expr(expr)
  return(expr_new)
}

#' Test if `set_mod_ctsm()` can set model without error
test_set_mod_ctsm <- function(){
  datf_para <- data.frame(
    "name" = c("Ti", "Te", "Ci", "Ce", "Rie", "Rea", "gA", "p11", "p22", "e11"),
    "init" = c("15", "10", "5", "0.5", "5", "0.5", "10", "0", "-1", "-1"),
    "lb" = c("0", "-25", "0.1", "0.1", "0.1", "0.1", "0.1", "-50", "-50", "-50"),
    "up" = c("25", "25", "20", "20", "50", "50", "40", "10", "10", "10")
  )
  mod <- set_mod_ctsm(
    c_expr_sys = c(
      d(Ti) ~ (1 / (Ci * Rie) * (Te - Ti) + gA / Ci * Ps + 1 / Ci * Ph) * d(t) +
        exp(p11) * d(w1),
      d(Te) ~ (1 / (Ce * Rie) * (Ti - Te) + 1 / (Ce * Rea) * (Ta - Te)) * d(t) +
        exp(p22) * d(w2)
    ),
    expr_obs = yTi ~ Ti,
    expr_error = yTi ~ exp(e11),
    c_input = c("Ta", "Ps", "Ph"),
    datf_para = datf_para
  )
  return("Pass")
}

