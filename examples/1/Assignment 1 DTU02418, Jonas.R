################################################################
############# Assignment 1 - Statistical Modelling #############
################################################################

# Initialization

# Clear variables
rm(list=ls())

# Clear plots
#dev.off()

# Load libaries
library("MASS")


## Descriptive statistics

tuno <- read.table("tuno.dat")

# Simple Models -----------------------------------------------------------

# Normalize power data and define wind data
normpow = (tuno$pow.obs)/5000
ws = tuno$ws30

# Define function that return negative log-likelihood for gamma distribution
GammaNLL <- function(pars, data){
  alpha <- pars[1]
  theta <- pars[2]
  return (-sum(dgamma(x = data, shape = alpha, scale = theta, log = TRUE)))
}

# Optimize alpha and theta parameters for windspeed and power
pow.gammafit <- optim(c(1, 1), fn = GammaNLL, gr = NULL, data = normpow)
ws.gammafit <- optim(c(1, 1), fn = GammaNLL, gr = NULL, data = ws)

# Define function that return negative log-likelihood for beta distribution
BetaNLL <- function(pars, data){
  alpha <- pars[1]
  beta <- pars[2]
  return (-sum(dbeta(x = data, shape1 = alpha, shape2 = beta, log = TRUE)))
}

# Optimize alpha and beta parameters for windspeed and power
pow.betafit <- optim(c(1, 1), fn = BetaNLL, gr = NULL, data = normpow)
# Sp??rg TA
#ws.betafit <- optim(c(1, 1), fn = BetaNLL, gr = NULL, data = ws)

# Define function that return negative log-likelihood for log normal distribution
LognormNLL <- function(pars, data){
  log_mean = pars[1]
  log_sd = pars[2]
  return (-sum(dnorm(x = data, mean = log_mean, sd = log_sd, log = TRUE)))
}

# Optimize mean (log scale) and standard deviation (log scale) parameters for windspeed and power
pow.lognormfit <- optim(c(0, 1), fn = LognormNLL, gr = NULL, data = normpow)
ws.lognormfit <- optim(c(0, 1), fn = LognormNLL, gr = NULL, data = ws)


### Plot fitted distribution densities with density of data ###

# Get density of data
den_pow = density(normpow, adjust = 0.7)
den_ws = density(ws, adjust = 0.7)
pow_x = seq( 0, 1.2, length.out = 5000)
ws_x = seq( 0, 26, length.out = 5000)

## Gamma distribution ##

# Power data
alpha = pow.gammafit$par[1]
theta = pow.gammafit$par[2]

gd = dgamma(pow_x, shape = alpha, scale = theta)
plot(den_pow$x, den_pow$y, type = 'l', xlim = c(0,1), ylim =c(0,3), col = 'blue')
lines(pow_x, gd, xlim = c(0,1), col = 'red')

# Windspeed data
alpha = ws.gammafit$par[1]
theta = ws.gammafit$par[2]

gd = dgamma(ws_x, shape = alpha, scale = theta)
plot(den_ws$x, den_ws$y, type = 'l', col = 'blue', xlim = c(0,25), ylim = c(0,0.11))
lines(ws_x, gd, col = 'red')


### Beta distribution ###

# Power data
alpha = pow.betafit$par[1]
beta = pow.betafit$par[2]

bd = dbeta(pow_x, shape1 = alpha, shape2 = beta)
plot(den_pow$x, den_pow$y, type = 'l', xlim = c(0,1), ylim =c(0,3), col = 'blue')
lines(pow_x, bd, xlim = c(0,1), col = 'red')

# Windspeed data
#alpha = ws.betafit$par[1]
#beta = ws.betafit$par[2]
#bd = density(rbeta(1000, shape1 = alpha, shape2 = beta))
#plot(den_ws$x, den_ws$y, type = 'l', col = 'blue', xlim = c(0,25), ylim = c(0,0.11))
#lines(bd$x, bd$y, col = 'red')

### Log normal distribution ###

# Power data
mean = pow.lognormfit$par[1]
std = pow.lognormfit$par[2]

ld = dnorm(pow_x, mean = mean, sd = std)
plot(den_pow$x, den_pow$y, type = 'l', xlim = c(0,1), ylim =c(0,3), col = 'blue')
lines(pow_x, ld, xlim = c(0,1), col = 'red')

# Windspeed data
mean = ws.lognormfit$par[1]
std = ws.lognormfit$par[2]

ld = dnorm(ws_x, mean = mean, sd = std)
plot(den_ws$x, den_ws$y, type = 'l', col = 'blue', xlim = c(0,25), ylim = c(0,0.11))
lines(ws_x, ld, col = 'red')



















# Regression Models -------------------------------------------------------

# Order wind speed observations to plot lines for fitted values and define wind direction
wsind <- order(ws)
ws_order <- ws[wsind]
wd = tuno$wd30
ws_sq = ws^2
ws_3 = ws^3

### Initial model (complex) ###
fit1 <- glm(normpow ~ ws + ws_sq + ws_3 + wd)
summary(fit1)

fit1_order <- fit1$fitted.values[wsind]

plot(ws, normpow)
lines(ws_order, fit1_order, col = 'blue')

qqnorm(fit1$residuals)
qqline(fit1$residuals)

x <- fit1$residuals #sample
h <- hist(x, plot=FALSE) #generate hist
plot(h, col="grey", main = 'Histogram of residuals', xlab = 'Residuals') #plot hist
xlines <-seq(min(h$breaks),max(h$breaks),length.out=100) #seq of x for pdf
lines(x = xlines,y=dnorm(xlines,mean(fit1$residuals),sd(fit1$residuals)) *length(x)*diff(h$breaks)[1], col = "lightcoral", lwd = 2)

### 2nd model (ws removed) ###
fit2 <- glm(normpow ~ ws_sq + + ws_3 + wd)
summary(fit2)

fit2_order <- fit2$fitted.values[wsind]

plot(ws, normpow)
lines(ws_order, fit2_order, col = 'blue')

qqnorm(fit2$residuals)
qqline(fit2$residuals)

x <- fit2$residuals #sample
h <- hist(x, plot=FALSE) #generate hist
plot(h, col="lightskyblue4", main = 'Histogram of residuals for initial model', xlab = 'Residuals') #plot hist
xlines <-seq(min(h$breaks),max(h$breaks),length.out=100) #seq of x for pdf
lines(x = xlines,y=dnorm(xlines,mean(fit2$residuals),sd(fit2$residuals)) *length(x)*diff(h$breaks)[1], col = "lightcoral", lwd = 2)

# Analysis of deviance table for model 1 and model 2 (ANOVA)
anova(fit1, fit2, test = 'Chisq')

### 3rd model (wd removed) ###
fit3 <- glm(normpow ~ ws_sq + ws_3)
summary(fit3)

fit3_order <- fit3$fitted.values[wsind]

plot(ws, normpow)
lines(ws_order, fit3_order, col = 'blue')

qqnorm(fit3$residuals)
qqline(fit3$residuals)

x <- fit3$residuals #sample
h <- hist(x, plot=FALSE) #generate hist
plot(h, col="lightskyblue4", main = 'Histogram of residuals for model 2', xlab = 'Residuals') #plot hist
xlines <-seq(min(h$breaks),max(h$breaks),length.out=100) #seq of x for pdf
lines(x = xlines,y=dnorm(xlines,mean(fit2$residuals),sd(fit2$residuals)) *length(x)*diff(h$breaks)[1], col = "lightcoral", lwd = 2)

anova(fit2, fit3, test = 'Chisq')

### 4th model (ws^4 added) ###
ws_4 <- ws^4
fit4 <- glm(normpow ~ ws_sq + ws_3 + ws_4)
summary(fit4)

fit4_order <- fit3$fitted.values[wsind]

plot(ws, normpow)
lines(ws_order, fit3_order, col = 'blue')

qqnorm(fit3$residuals)
qqline(fit3$residuals)

x <- fit3$residuals #sample
h <- hist(x, plot=FALSE) #generate hist
plot(h, col="lightskyblue4", main = 'Histogram of residuals for model 3', xlab = 'Residuals') #plot hist
xlines <-seq(min(h$breaks),max(h$breaks),length.out=100) #seq of x for pdf
lines(x = xlines,y=dnorm(xlines,mean(fit2$residuals),sd(fit2$residuals)) *length(x)*diff(h$breaks)[1], col = "lightcoral", lwd = 2)

anova(fit2, fit3, test = 'Chisq')

### FINAL MODEL : f(pow) = b0 + b1 * ws^2 + b2 * ws^3 ###

# Parameters
b0 = coefficients(fit3)[1]
b1 = coefficients(fit3)[2]
b2 = coefficients(fit3)[3]

# Uncertainty
b0_conf = confint(fit3, "(Intercept)", level = 0.95, trace = FALSE)
b1_conf = confint(fit3, "ws_sq", level = 0.95, trace = FALSE)
b2_conf = confint(fit3, "ws_3", level = 0.95, trace = FALSE)

conf_int <- function(ws){
  return (cbind(b0_conf[1] + b1_conf[1]*ws^2 + b2_conf[1]*(ws^3),
                (b0_conf[2] + b1_conf[2]*ws^2 + b2_conf[2]*(ws^3)))
  )
}

fit3_conf <- conf_int(ws_order)
fit3_low <- fit3_conf[,1]
fit3_high <- fit3_conf[,2]

plot(ws, normpow*5000, xlab = 'Windspeed (m/s)', ylab = 'Average power production (kw)')
lines(ws_order, fit3_order*5000, col = 'blue')
lines(ws_order, fit3_low*5000, col= 'red', lty = 'dashed')
lines(ws_order, fit3_high*5000, col = 'red', lty = 'dashed')

legend('topleft', legend=c("Observations", "Fitted values", "95% Confidence Interval"),
       col=c("black", "blue", "red"), lty = c(NA, 1, 2), pch = c(1,NA,NA), cex = 1,
       bty = "n")


### TESTS ###

X = cbind(numeric(length(normpow)) + 1, ws_sq, ws_3)
Y = normpow

beta <- solve((t(X) %*% X),(t(X)%*%Y))



fit <- glm(normpow ~ ws_sq + ws_3)
summary(fit)

conf_int <- predict(fit, interval = "confidence", level = 0.95)

pred_int <- predict(lm(normpow ~ ws_sq + ws_3), interval = "prediction", level = 0.95)


plot(ws, normpow*5000, xlab = 'Windspeed (m/s)', ylab = 'Average power production (kw)', col = "black")
lines(ws_order, conf_int[,1][wsind]*5000, col = 'lightcoral', lwd = 2)
lines(ws_order, conf_int[,2][wsind]*5000, col= 'lightskyblue4', lty = 'dashed', lwd = 2)
lines(ws_order, conf_int[,3][wsind]*5000, col = 'lightskyblue4', lty = 'dashed', lwd = 2)
lines(ws_order, pred_int[,2][wsind]*5000, col= 'chocolate4', lty = 'dashed', lwd = 2)
lines(ws_order, pred_int[,3][wsind]*5000, col = 'chocolate4', lty = 'dashed', lwd = 2)


legend('topleft', legend=c("Observations", "Fitted values", "95% Confidence Interval", "95% Prediction Interval"),
       col=c("black", "lightcoral", "lightskyblue4", "chocolate4"), lty = c(NA, 1, 2, 2), pch = c(1,NA,NA, NA), cex = 1,
       bty = "n", lwd = c(NA, 2, 2, 2))






