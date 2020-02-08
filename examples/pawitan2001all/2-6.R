y <- c(0.5, -0.32, -0.55, -0.76, -0.07, 0.44, -0.48)

lla.f <- function(theta, y) {
  prod(dunif(y, theta - 1, theta + 1))
}

llb.f <- function(theta, y) {
  prod(dunif(y, -theta, theta))
}

llc.f <- function(theta, y) {
  prod(dnorm(y, sd = sqrt(theta)))
}


n <- length(y)
par(mfrow = c(2, 2))
# 1)
theta <- seq(-1, 1, by = 0.01)
lla <- sapply(theta, lla.f, y = y)
plot(theta, lla / max(lla), type = "l")

# 2)
theta <- seq(0.01, 2, by = 0.01)
llb <- sapply(theta, llb.f, y = y)
plot(theta, llb / max(llb), type = "l")

# 3)
theta <- seq(0.01, 2, by = 0.01)
llc <- sapply(theta, llc.f, y = y)
plot(theta, llc / max(llc), type = "l")