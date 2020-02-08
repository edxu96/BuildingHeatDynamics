## (i)
## a-b
mle.a <- mean(y)

## c
mle.c <- optimize(llc.f, c(70, 90), y = median(y), sigma = sigma, maximum = TRUE)$maximum

## d
mle.d <- optimize(lld.f, c(70, 90),
                  y = c(min(y), max(y)), sigma = sigma,
                  maximum = TRUE
)$maximum

## e
mle.e <- optimise(lle.f, c(70, 90),
                  y = sort(y)[1:2], sigma = sigma,
                  maximum = TRUE
)$maximum


library(numDeriv)

## Fisher information
## a-b
length(y) / sigma^2

## c
func <- function(x) {
  log(llc.f(x, y = median(y), sigma = sigma))
}
(FIc <- -hessian(func, mle.c))

## d
func <- function(x) {
  log(lld.f(x, y = c(min(y), max(y)), sigma = sigma))
}
(FId <- -hessian(func, mle.d))

## e
func <- function(x) {
  log(lle.f(x, y = sort(y)[1:2], sigma = sigma))
}
(FIe <- -hessian(func, mle.e))



#' Calculate Likelihood based confidense intervals
#' 
cal_lkci <- function(y, sigma, limits_two, alpha, lkf) {
  ## Get the MLE and its corresponding maximum likelihood 
  est_mle <- optimize(ll.f, limits, y, sigma, maximum = TRUE)
  mlk <- est_mle$objective
  mle <- est_mle$maximum
  
  fun.tmp <- function(x, y, sigma) {
    log(mlk) - log(lkf(x, y, sigma)) - qchisq(1 - alpha, 1) / 2
  }
  
  lb <- uniroot(fun.tmp, interval = c(mle, limits[2]), y, sigma)$root
  ub <- uniroot(fun.tmp, interval = c(limits[1], mle), y, sigma)$root
  
  return(c(lb = lb, mle = mle, ub = ub))
}

## In case 1 and 2 compare with usual confidence intervals
## note that the likelihod based interval equal the usual large
## sample t.test confidence interval
limits <- range(y)
alpha <- 0.01
confInt1 <- cal_lkci(y, sigma, limits, alpha, lla.f)
confInt1
mean(y) + c(-1, 1) * qnorm(0.995) * sd(y) / sqrt(length(y))
mean(y) + c(-1, 1) * qt(0.995, df = 10) * sd(y) / sqrt(length(y))

confInt3 <- cal_lkci(sort(y)[6], sigma, limits, alpha, llc.f)
confInt4 <- cal_lkci(sort(y)[c(1, 11)], sigma, limits, alpha, lld.f)
confInt5 <- cal_lkci(sort(y)[c(1, 2)], sigma, limits, alpha, lle.f)

confInt1
confInt3
confInt4
confInt5

## Wald CI
## a-b
confInt1
mean(y) + c(-1, 1) * qnorm(0.995) * sqrt(sigma^2 / length(y))

## c
confInt3
mean(y) + c(-1, 1) * qnorm(0.995) * sqrt(1 / FIc)


## d
confInt4
mean(y) + c(-1, 1) * qnorm(0.995) * sqrt(1 / FId)

## e
confInt5
mean(y) + c(-1, 1) * qnorm(0.995) * sqrt(1 / FIe)

