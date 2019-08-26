## ----Initialize-------------------------------------------------
## Remove all in memory
rm(ls = list())

## Load the data
datf_x <- readRDS("soenderborg_2day.RDS")

## Load the splines package
library(splines)
source("./func_nonpara.R")

optim_reg_local(bandwidth = 10, datf_x = datf_x)

result <- optimize(optim_reg_local, lower = 1, upper = 100, datf_x = datf_x, maximum = FALSE)
result

## ----The estimated function-------------------------------------
## Plot and see the function fitted with the optimal bandwidth
h <- result$minimum

## Fit a locally weighted model for a sequence
x = seq(min(datf_x$Te), max(datf_x$Te), len=100)

## A vector for the predictions
P_hat <- rep(NA, length(x))
## Fit and predict for each point
for(i in 1:length(x)){
    fit <- lm(P ~ Te, data = datf_x, weight = tri(x[i], datf_x$Te, h=h))
    P_hat[i] <- predict(fit, newdata = data.frame(Te=x[i]))
}
## Plot it
plot(datf_x$Te, datf_x$P)
lines(x, P_hat)



## ----BS cross-validation----------------------------------------
## For the optimal df for the spline model using cross-validation
##
## Wrap it in a function for an optimizer
obj <- function(df, datf_x){
    P_hat <- rep(NA, nrow(datf_x))
    for(i in 1:nrow(datf_x)){
        vec_sub_i <- (1:nrow(datf_x))[-i]
        ## The i'th observation will not be included in the fit
        fit <- lm(P ~ bs(Te, df=df), data = datf_x, subset = vec_sub_i)
        ## Now predict for the i'th observation
        P_hat[i] <- predict(fit, newdata = data.frame(Te=datf_x$Te[i]))
    }
    ## The score value
    val <- rmse(datf_x$P - P_hat)
    ##
    print(paste("df =",df,", val =",val))
    ##
    return(val)
}

for(df in 3:10){
    obj(df, datf_x)
}



## ----BS selection score-----------------------------------------
## Or use AIC
for(df in 3:10){
    ## Just fit it
    fit <- lm(P ~ bs(Te, df=df), data = datf_x)
    ## Calculate the AIC
    print(paste("df =",df,", AIC =",AIC(fit)))
}


## Or use BIC (punish larger models more than AIC)
for(df in 3:10){
    ## Just fit it
    fit <- lm(P ~ bs(Te, df=df), data = datf_x)
    ## Calculate the AIC
    print(paste("df =",df,", BIC =",BIC(fit)))
}
