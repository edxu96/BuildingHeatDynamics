## ----------------------------------------------------------------
rm(list = ls())
setwd("~/GitHub/MatrixTSA")
sapply(
  dir("./examples_source/prediction/functions", full.names = TRUE), source
)

## A function for fitting a recursive least squares estimation
rls <- function(formula, lambda, data, k) {
  ## R build-in function for setting up linear regression model
  mf <- model.frame(formula, data, na.action = na.pass)
  ## The model output
  y <- mf[, 1]
  ## The design matrix
  datf <- model.matrix(formula, mf)

  ## The number of observations
  n <- nrow(datf)
  ## The number of parameters
  p <- ncol(datf)
  ## Parameter matrix
  Theta <- matrix(as.numeric(NA), nrow = n, ncol = p)
  ## The predictions
  yhat <- as.numeric(rep(NA, n))
  ## The parameter vector
  theta <- matrix(rep(0, p), ncol = 1)

  ## Start value for the parameter covariance P
  P <- 10000 * diag(1, p)
  ## Use the inverse in the RLS
  R <- solve(P)

  ## Iterate through and estimate the parameters
  for (i in 1:(n - k)) {
    x <- matrix(datf[i, ])
    ## Check for NAs in inputs and output
    if (all(!is.na(x)) & !is.na(y[i])) {
      ## Update
      R <- lambda * R + x %*% t(x)
      theta <- theta + solve(R, x) %*% (y[i] - t(x) %*% theta)
      Theta[i, ] <- t(theta)
    }
    ## Predict
    x <- matrix(datf[i + k, ])
    if (all(!is.na(x))) {
      yhat[i + k] <- t(x) %*% theta
    }
  }

  ## Return a list
  L <- list()
  L$residuals <- y - yhat
  L$datf <- datf
  L$y <- y
  L$Theta <- Theta
  return(L)
}

## Generate some data from a linear model
n <- 200
x <- runif(n)
beta0 <- 2
beta1 <- -3
y <- beta0 + beta1 * x + rnorm(n, sd = 0.1)

## Try to estimate the parameters
fit <- lm(y ~ x)
plot(x, y)
abline(fit)
fit

## Change the coefficients and generate more data
x1 <- runif(n)
beta0 <- -2
beta1 <- 3
y1 <- beta0 + beta1 * x1 + rnorm(n, sd = 0.1)

## Put together, such that at the start of the the second part, i=201 the
## coefficients change and have different values
datf <- data.frame(y = c(y, y1), x = c(x, x1))

## See clearly there is two "regimes"
plot(datf$x, datf$y)

## Here you only see the change in intercept
plot(datf$y)

## Fit a linear regression on the binded data
lm(y ~ x, datf)

## Fit a recursive linear regression on the binded data
val <- rls(y ~ x, lambda = 0.97, data = datf, k = 1)

## Plot the tracked parameters
plot.ts(val$Theta)
## ----------------------------------------------------------------
