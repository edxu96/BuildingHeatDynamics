
library(tidyverse)

cal_ll_o <- list()

cal_ll_o$a <- function(theta, y_i, sigma) {
  ll <- prod(dnorm(y_i, mean = theta, sd = sigma))
  return(ll)
  }

cal_ll_o$b <- function(theta, y_bar, sigma) {
  ll <- prod(dnorm(y_bar, mean = theta, sd = sigma))
  return(ll)
  }

cal_ll_o$c <- function(theta, y_i, sigma) {
  pnorm(y_i, mean = theta, sd = sigma)^5 *
  (1 - pnorm(y_i, mean = theta, sd = sigma))^5 *
  dnorm(y_i, mean = theta, sd = sigma)
}

cal_ll_o$d <- function(theta, y_i, sigma) {
  (pnorm(y_i[2], mean = theta, sd = sigma) -
  pnorm(y_i[1], mean = theta, sd = sigma))^9 *
  dnorm(y_i[2], mean = theta, sd = sigma) *
  dnorm(y_i[1], mean = theta, sd = sigma)
}

cal_ll_o$e <- function(theta, y_i, sigma) {
  (1 - pnorm(y_i[2], mean = theta, sd = sigma))^9 *
  dnorm(y_i[1], mean = theta, sd = sigma) *
  dnorm(y_i[2], mean = theta, sd = sigma)
}

y_i <- 
  tibble(y = c(73, 75, 84, 76, 93, 79, 85, 80, 76, 78, 80))

sigma <- sd(y_i$y)
theta_k <- seq(min(y_i) - 3, max(y_i) + 3, by = 0.01)


n <- length(y_i)
par(mfrow = c(1, 1))

ll_o <- list()

ll_o$a <- 
  tibble(theta = theta_k) %>%
  add_column(ll = sapply(theta_k, cal_ll_o$a, y_i = y_i$y, sigma = sigma))

plot(theta_k, ll_o$a / max(ll_o$a), type = "l")

ll_o$b <- sapply(theta_k, cal_ll_o$b, y = mean(y_i), sigma = sigma / sqrt(n))
lines(theta_k, ll_o$b / max(ll_o$b), col = 2, type = "l")

ll_o$c <- sapply(theta_k, cal_ll_o$c, y = median(y_i), sigma = sigma)
lines(theta_k, llc / max(llc), col = 3)

ll_o$d <- sapply(theta_k, lld.f, y_i = c(min(y_i), max(y_i)), sigma = sigma)
lines(theta_k, lld / max(lld), col = 4)

ll_o$e <- sapply(theta_k, lle.f, y_i = sort(y_i)[1:2], sigma = sigma)
lines(theta_k, lle / max(lle), col = 5)

rug(y_i)
lines(mean(y_i) * c(1, 1), c(-1, 2), col = 2, lty = 2)
lines(median(y_i) * c(1, 1), c(-1, 2), col = 3, lty = 2)
lines((diff(range(y_i)) / 2 + min(y_i)) * c(1, 1), c(-1, 2), col = 4, lty = 2)
