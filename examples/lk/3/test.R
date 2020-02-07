

```{r}
pois.HMM.pn2pw <- function(numDist, lambda, gamma)
{                                              
    tlambda <- log(lambda)                         
    tgamma  <- NULL                              
    if (numDist > 1) {                                            
        foo   <- log(gamma / diag(gamma))           
        tgamma<- as.vector(foo[!diag(numDist)])             
    }                                             
    vecPars <- c(tlambda, tgamma)                    
    vecPars                                         
}  

pois.HMM.pw2pn <- function(numDist, vecPars)                 
{                                                     
    epar   <- exp(vecPars)                              
    lambda <- epar[1: numDist]                                   
    gamma  <- diag(numDist)                                    
    if (numDist > 1) {                                                  
        gamma[!gamma] <- epar[(numDist+1):(numDist*numDist)]                  
        gamma         <- gamma/apply(gamma,1,sum)          
    }                                                   
    delta  <- solve(t(diag(numDist)-gamma+1),rep(1,numDist))          
    list(lambda=lambda,gamma=gamma,delta=delta)           
}  


pois.HMM.mllk <- function(vecPars,x,numDist,...)       
{
    #    print(vecPars)
    if(numDist==1) return(-sum(dpois(x,exp(vecPars),log=TRUE))) 
    n          <- length(x)                            
    parsNatural         <- pois.HMM.pw2pn(numDist,vecPars)            
    allprobs   <- outer(x,parsNatural$lambda,dpois)             
    allprobs   <- ifelse(!is.na(allprobs),allprobs,1)  
    scaleL     <- 0                                    
    foo        <- parsNatural$delta                             
    for (i in 1:n) {                                                
        foo    <- foo%*%parsNatural$gamma*allprobs[i,]            
        sumfoo <- sum(foo)                               
        scaleL <- scaleL+log(sumfoo)                    
        foo    <- foo/sumfoo                            
    }                                               
    mllk       <- -scaleL                            
    mllk                                              
}   
```


```{r}
## natural to working parameters
poisMix.pn2pw <- function(numDist, lambda, delta){
    # length(lambda) = numDist
    # length(delta) = numDist - 1
    if(sum(delta) >= 1){print("sum(delta) should be < 1")
        return()
    }
    if(length(lambda) != numDist){
        print("length(lambda) should be numDist")
        return()
    }
    if(length(delta) != (numDist - 1)){
        print("length(delta) should be numDist - 1")
        return()
    }
    eta <- log(lambda)
    tau <- log(delta / (1 - sum(delta)))
    return(list(eta = eta, tau = tau))
}

## Poisson mixture: transform
## working to natural parameters
poisMix.pw2pn <- function(numDist, eta, tau){
    if(numDist == 1){return(exp(eta))}
    if(length(eta) != numDist){
        print("length(lambda) should be numDist")
        return()}
    if(length(tau) != (numDist-1)){
        print("length(delta) should be numDist-1")
        return()
    }
    lambda <- exp(eta)
    delta <- exp(tau) / (1 + sum(exp(tau)))
    delta <- c(1 - sum(delta), delta)
    return(list(lambda = lambda, delta = delta))
}

## negative log likelihood function
calNegaLogL <- function(theta, numDist, y){
    if(numDist == 1){
        return(- sum(dpois(y, lambda = exp(theta), log = TRUE)))
    }
    eta <- theta[1: numDist]
    tau <- theta[(numDist + 1): (2 * numDist - 1)]
    parsNatural <- poisMix.pw2pn(numDist, eta, tau)
    n <- length(y)
    negaLogL <- 0  # negative log likelihood
    for(i in 1:n){
        negaLogL <- negaLogL - log(sum(parsNatural$delta * dpois(y[i], lambda = parsNatural$lambda)))
    }
    return(negaLogL)
}
```



```{r}
numDist <- 1
lambda <- mean(y)
delta <- c()
parsWork <- poisMix.pn2pw(numDist, lambda, delta)
theta <- c(parsWork$eta, parsWork$tau)
calNegaLogL(theta, numDist, y)
opt_mixNorm_1 <- nlminb(theta, calNegaLogL, numDist = numDist, y = y)
parsNatural_1 <- poisMix.pw2pn(numDist, opt_mixNorm_1$par[1: numDist], opt_mixNorm_1$par[(numDist + 1): (2 * numDist - 1)])
```

```{r}
numDist_2 <- 2
lambda_2 <- c(1 / 2, 3 / 2) * mean(y)
delta_2 <- c(0.5)
parsWork_2 <- poisMix.pn2pw(numDist_2, lambda_2, delta_2)
theta_2 <- c(parsWork_2$eta, parsWork_2$tau)
calNegaLogL(theta_2, numDist_2, y)
opt_mixNorm_2 <- nlminb(theta_2, calNegaLogL, numDist = numDist_2, y = y)
parsNatural_2 <- poisMix.pw2pn(numDist,opt_mixNorm_2$par[1: numDist_2], opt_mixNorm_2$par[(numDist + 1): (2 * numDist - 1)])
```

```{r}
numDist_3 <- 3
lambda_3 <- c(1 / 2, 1, 3 / 2) * mean(y)
delta_3 <- c(1, 1) / 3
parsWork_3 <- poisMix.pn2pw(numDist_3, lambda_3, delta_3)
theta_3 <- c(parsWork_3$eta, parsWork_3$tau)
calNegaLogL(theta_3, numDist_3, y)
opt_mixNorm_3 <- nlminb(theta_3, calNegaLogL, numDist = numDist_3, y = y)
parsNatural_3 <- poisMix.pw2pn(numDist_3, opt_mixNorm_3$par[1: numDist_3], opt_mixNorm_3$par[(numDist_3 + 1): (2 * numDist_3 - 1)])

c(opt_mixNorm_1$objective, opt_mixNorm_2$objective, opt_mixNorm_3$objective)
```

```{r, include = FALSE}
## EM Algorithm for mixture of two normal distribution
#  Reference: 8.5 The EM Algorithm in P272, The Element of Statistical Learning
cal_gamma <- function(pi, mu1, sigma1, mu2, sigma2, seq_y){
    gamma <- rep(0, length(seq_y))
    i <- 1
    for(y in seq_y){
        gamma[i] <- pi * dnorm(y, mean = mu2, sd = sigma2) / ((1 - pi) * dnorm(y, mean = mu1, sd = sigma1) + pi * dnorm(y, mean = mu2, sd = sigma2))
        i <- i + 1
    }
    return(gamma)
}
cal_mu1.next <- function(gamma, seq_y){
    i <- 1
    sum_1 <- 0
    sum_2 <- 0
    for(y in seq_y){
        sum.new_1 <- (1 - gamma[i]) * y
        sum_1 <- sum_1 + sum.new_1
        sum.new_2 <- (1 - gamma[i])
        sum_2 <- sum_2 + sum.new_2
        i <- i + 1
    }
    return(sum_1 / sum_2)
}
cal_mu2.next <- function(gamma, seq_y){
    i <- 1
    sum_1 <- 0
    sum_2 <- 0
    for(y in seq_y){
        sum.new_1 <- gamma[i] * y
        sum_1 <- sum_1 + sum.new_1
        sum.new_2 <- gamma[i]
        sum_2 <- sum_2 + sum.new_2
        i <- i + 1
    }
    return(sum_1 / sum_2)
}
cal_sigma1.next <- function(mu1.next, gamma, seq_y){
    i <- 1
    sum_1 <- 0
    sum_2 <- 0
    for(y in seq_y){
        sum.new_1 <- (1 - gamma[i]) * (y - mu1.next)^2
        sum_1 <- sum_1 + sum.new_1
        sum.new_2 <- (1 - gamma[i])
        sum_2 <- sum_2 + sum.new_2
        i <- i + 1
    }
    return(sum_1 / sum_2)
}
cal_sigma2.next <- function(mu2.next, gamma, seq_y){
    i <- 1
    sum_1 <- 0
    sum_2 <- 0
    for(y in seq_y){
        sum.new_1 <- gamma[i] * (y - mu2.next)^2
        sum_1 <- sum_1 + sum.new_1
        sum.new_2 <- gamma[i]
        sum_2 <- sum_2 + sum.new_2
        i <- i + 1
    }
    return(sum_1 / sum_2)
}
cal_pi.next <- function(gamma, seq_y){
    i <- 1
    sum <- 0
    for(y in seq_y){
        sum.new <- gamma[i] / length(seq_y)
        sum <- sum + sum.new
        i <- i + 1
    }
    return(sum)
}

seq_y <- y
##
pi.next <- 0.5
mu1.next <- dnorm.optim_1$par[1]
sigma1.next <- dnorm.optim_1$par[2]
mu2.next <- dnorm.optim_1$par[1] + 0.001
sigma2.next <- dnorm.optim_1$par[2] + 0.001
gamma.next <- cal_gamma(pi.next, mu1.next, sigma1.next, mu2.next, sigma2.next, seq_y)
mu1 <- mu1.next - 0.0001
j <- 1
while (abs(mu1.next - mu1) > 10^-6) {
    gamma <- gamma.next
    mu1 <- mu1.next
    sigma1 <- sigma1.next
    mu2 <- mu2.next
    sigma2 <- sigma2.next
    pi <- pi.next
    # Calculate new
    gamma.next <- cal_gamma(pi, mu1, sigma1, mu2, sigma2, seq_y)
    mu1.next <- cal_mu1.next(gamma.next, seq_y)
    sigma1.next <- cal_sigma1.next(mu1.next, gamma.next, seq_y)
    mu2.next <- cal_mu2.next(gamma.next, seq_y)
    sigma2.next <- cal_sigma2.next(mu2.next, gamma.next, seq_y)
    pi.next <- cal_pi.next(gamma.next, seq_y)
    j <- j + 1
}
```

```{r}
library("depmixS4")
hmmDnorm.2 <- depmix(response = dataETF.f$x ~ 1, data = dataETF.f$SPY, nstates = 2, trstart = runif(4), family = gaussian())
optHmmDnorm.2 <- fit(hmmDnorm.2)
```