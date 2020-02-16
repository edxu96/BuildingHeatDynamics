## Main R file for DTU42008 Econometrics Exercise 2
## Edward J. Xu
## Feb 12, 2020

library(tidyverse)

ti <-
  read_csv("./eeha.csv") %>%
  mutate(id = respondent_ID, invest = invest_EE) %>%
  select(id, invest)

## MLE ####
## Compute the MLE and estimate the corresponding standard error

lkf <- function(theta, invest){
  z_bar <- mean(invest)
  lkf <- theta^z_bar * (1 - theta)^z_bar
  
  return(lkf)
}

theta_hat <- mean(ti$invest)
n <- nrow(ti)
se_hat <- sqrt(theta_hat * (1 - theta_hat)) / sqrt(n)

