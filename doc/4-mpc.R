## ---- include=FALSE------------------------------------------------------
rm(list = ls())
knitr::opts_chunk$set(
  collapse = TRUE, 
  comment = "#>", 
  fig.show = "hold", 
  fig.width = 7, 
  fig.retina = T,
  fig.asp = 9/16
  )

## ---- message=F, include=FALSE-------------------------------------------
library(tidynamics)
library(tibble)
library(tidyverse)
library(gridExtra)
library(egg)

source("~/GitHub/tidynamics/vignettes/funcs/mpc/asHours.R")
source("~/GitHub/tidynamics/vignettes/funcs/mpc/asP.R")
source("~/GitHub/tidynamics/vignettes/funcs/mpc/prbs.R")
source("~/GitHub/tidynamics/vignettes/funcs/mpc/sim_building.R")

## ---- message=FALSE------------------------------------------------------
ti <- 
  read_csv(
    "~/GitHub/tidynamics/data/mpc.csv",
    skip = 1,
    col_names = c("timedate", "Y1", "Y2", "Ta", "Gv", "Ph1", "Ph2")
  ) %>%
  mutate(timedate = asP(.$timedate)) %>%
  mutate(t = asHours(.$timedate - .$timedate[1]))

ti %>%
  print()

## ------------------------------------------------------------------------
ti_est <- tibble(
  name = c(
    "T1a0", "T1m0", "C1a", "C1m", "R1a",
    "R1m", "A1w", "p1", "p1a", "p1m", "e11"
  ),
  init = c(25, 25, 6, 12, 10, 1, 1, 0.5, 1, 1, -1),
  lb = c(0, 0, 1E-5, 1, 1, 1E-10, 1E-10, 0, -30, -30, -50),
  up = c(35, 35, 20, 50, 80, 10, 10, 1, 10, 10, 10)
)

li_mod <- list()

li_mod[[1]] <- set_mod_ctsm(
  c_expr_sys = c(
    d(T1a) ~ (
        1 / (C1a * R1m) * (T1m - T1a) + 1 / (C1a * R1a) * (Ta - T1a) + 
        1 / C1a * Ph1 + p1 * A1w / C1a * Gv
      ) * d(t) + exp(p1a) / C1a * d(w1a),
    d(T1m) ~ (
        1 / (C1m * R1m) * (T1a - T1m) + (1 - p1) * A1w / C1m * Gv
      ) * d(t) + exp(p1m) / C1m * d(w1m)
    ),
  expr_obs = Y1 ~ T1a,
  expr_var = Y1 ~ exp(e11),
  c_input = c("Ta", "Ph1", "Gv"),
  ti_est = ti_est
)

## ---- results="hide", include=FALSE--------------------------------------
fit1 <- 
  li_mod[[1]] %>%
  est_mod_ctsm(ti_data = ti)

## ------------------------------------------------------------------------
li_mat_ss <- 
  fit1 %>%
  trans_ctsm_ss()

li_mat_ss_d <- 
  li_mat_ss %>%
  trans_mat_ss(ti_data = ti)

## ---- results=F----------------------------------------------------------
result <- sim_building(li_mat_ss, li_mat_ss_d, ti)
Tall <- result$Tall
Ymin <- result$Ymin
Ymax <- result$Ymax
Price <- result$Price
Tmax <- result$Tmax
u <- result$u

ti_result <- tibble(
  t = 1:length(u),
  tall1 = Tall[1,],
  tall2 = Tall[2,],
  u = u
  )
ti_p <- tibble(
  t = 1:length(result$Price),
  p = result$Price
)

