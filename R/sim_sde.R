# Simulation and Analysis of SDEs
# Edward J. Xu
# Aug 28, 2019

library(yuima)

mod_1 <- setModel(drift = "-3 * x", diffusion = "1 / (1 + x^2)")
set.seed(123)
sim_1 <- simulate(mod_1)
plot(sim_1)
