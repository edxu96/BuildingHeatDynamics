## a
lambda <- 1 / seq(1, 1000, by = 0.1)
plot(lambda, 1 - pexp(30, lambda), type = "l")


## b

ll <- function(lambda) {
  (1 - pexp(30, lambda))^8 * dexp(6, lambda) * dexp(28, lambda)
}

lambda <- c(1e-5, seq(0, 0.04, by = 0.0001))
ml <- max(ll(lambda))
plot(lambda, ll(lambda) / ml, type = "l")


## c

optimise(ll, c(0, 0.05), maximum = TRUE)

## ll = 8 * log(1 - pexp(30, lambda)) + log(dexp(6, lambda)) +
##     log(dexp(28, lambda))
##  = 8 * 30 * lambda - 2 * log(lambda) + (6+28)*lambda
##
## ll' = 274 -2/lambda
## lambda.hat = 2/274

optimise(ll, c(0, 0.05), maximum = TRUE)$maximum - 1 / 137

## d)
qexp(0.9, 1 / 137)

## e)

ll.e <- function(lambda) {
  (1 - pexp(30, lambda))^8 * pexp(20, lambda)^2
}

lambda <- c(1e-5, seq(0, 0.04, by = 0.0001))
ml <- max(ll.e(lambda))
lines(lambda, ll.e(lambda) / ml, type = "l", col = 2)