## Functions for Linear Stochastic System
## Edward J. Xu
## April 25th, 2019
########################################################################################################################
## 1,  Functions to Perform Statistical Test
#' 1.1,  Function to perform Likelihood ratio test
#' Example: testLogLikRatio(mod_3.1.4, mod_3.1.3)
#' @param mod_ref 
#' @param mod 
#' @return value
#' @references 
testLogLikRatio <- function(mod_ref, mod){
    # mod_ref$loglik - mod$loglik
    # don't use "1 - pchisq(-2 * ( fit1$loglik - fit2$loglik ), df = 1)", which will return 0 most of the time because 
    # of numerical approximation
    value <- pchisq(- 2 * (mod_ref$loglik - mod$loglik), df = 1, lower.tail = FALSE)
    return(value)
}
# 1.2,  Function to perform F-test of model against reference model
# Example: testFDist(mod_3.1.4, mod_3.1.3)
testFDist <- function(mod_ref, mod){
    sum_residual_ref <- sum(mod_ref$residuals^2)
    sum_residual <- sum(mod$residuals^2)
    num_para_ref <- length(mod_ref$coef)
    num_para <- length(mod$coef)
    stat_f <- (sum_residual_ref - sum_residual) / (num_para - num_para_ref) / 
        (sum_residual / (length(mod_ref$residuals) - num_para)) # If num_para = num_para_ref, it will be Inf
    value <- pf(stat_f, df1 = num_para - num_para_ref, df2 = (length(mod_ref$residuals) - num_para), 
                lower.tail = FALSE)    
    return(value)
}
#' 1.3,  Function to Perform Test to a List of Models
testModel <- function(list_mod, vec_name = seq(1, length(list_mod))){
    num_mod <- length(list_mod)
    boolean <- T
    if (length(vec_name) == num_mod){
        vec_type <- rep("wrong", num_mod)
        vec_str_coef <- rep("wrong", num_mod)
        vec_aic <- rep(0, num_mod)
        vec_test1 <- rep(0, num_mod)
        vec_test2 <- rep(0, num_mod) 
        for (i in 1: num_mod){
            vec_type[i] <- paste(list_mod[[i]])
            vec_str_coef[i] <- paste(round(list_mod[[i]]$coef, digits = 4), collapse=", ")
            vec_aic[i] <- list_mod[[i]]$aic
            vec_test1[i] <- testLogLikRatio(list_mod[[1]], list_mod[[i]])
            vec_test2[i] <- testFDist(list_mod[[1]], list_mod[[i]])
        }
        # Form the table
        table <- data.frame(vec_name, vec_type, vec_str_coef, vec_aic, vec_test1, vec_test2)
        return(table)
    } else {
        return("Wrong number of names!")
    }
}
########################################################################################################################
## 2,  Functions to Plot the Result
# 2.1,  Plot the time series / residuals and its ACF, PACF, and Ljung-Box plot
plotTimeSeriesResidual <- function(dat, num_lag = 20, str_name = "Model", ...){
    # If the input dat is arima model, just take its residuals are the dat.
    if(class(dat) == "Arima")
        dat <- dat$residuals
    par(mfrow = c(4, 1), mgp = c(2, 0.7, 0), mar = c(1, 1, 1, 1), oma = c(2, 2, 4, 2), 
        cex.lab = 0.8, cex.axis = 0.8)
    plot(dat, type = "l", bty = "n")
    title(main = paste("Residuals, ACF, PACF and p Values for Ljung-Box Statistic of", str_name), 
          cex.main = 1.5, line = 1, outer = TRUE)
    # ACF and PACF
    acf(dat, lag.max = num_lag, xaxt = "n", bty = "n")
    axis(1, at = seq(0, num_lag, by = 1), las = 0, outer = FALSE)
    pacf(dat, lag.max = num_lag, xaxt = "n", bty = "n")
    axis(1, at = seq(1, num_lag, by = 1), las = 0, outer = FALSE)
    # Ljung-Box Plot
    value_p <- sapply(1: num_lag, function(i) Box.test(dat, i, type = "Ljung-Box")$p.value)
    plot(1L: num_lag, value_p, xlab = "lag", ylab = "p value", ylim = c(0, 1), bty = "n", xaxt = "n")
    axis(1, at = seq(1, num_lag, by = 1), las = 0, outer = FALSE)
    abline(h = 0.05, lty = 2, col = "blue")
}
#' 2.2,  Plot the time series and its ACF, PACF
plotTimeSeries <- function(dat, num_lag = 20, str_name = "Model", ...){
    par(mfrow = c(2, 1), mgp = c(2, 0.7, 0), mar = c(1, 1, 1, 1), oma = c(2, 2, 4, 2), 
        cex.lab = 0.8, cex.axis = 0.8)
    # ACF and PACF
    acf(dat, lag.max = num_lag, xaxt = "n", bty = "n")
    axis(1, at = seq(0, num_lag, by = 5), las = 0, outer = FALSE)
    pacf(dat, lag.max = num_lag, xaxt = "n", bty = "n")
    axis(1, at = seq(1, num_lag, by = 5), las = 0, outer = FALSE)
    title(main = paste("Auto-Correlation Function (ACF) and Partial-ACF of", str_name), line = 1, outer = TRUE)
}
#' 2.3,  Function to Plot the CCF
plotCCFunc <- function(x, y, name_x, name_y, num_lag = 100){
    par(mar=c(3.5,3.5,3,1), mgp=c(2,0.7,0))
    ccf(x, y, type = "correlation", lag.max = num_lag, ylab = "CCF", bty = "num_obs", na.action = na.pass,
        main = paste("Cross-Correlation Function of x (", name_x, ") and y (", name_y, ")", sep = ""))
    # axis(1, at = seq(0, num_lag, by = 5), las = 0, outer = FALSE)
}
#' 2.4,  Function to Calculate Prediction Interval from Standard Error
calIntervalPred <- function(n, p, y.hat, se, prob = 0.95){
    quantileStudentDist <- qt(p = 0.95, df = n - p)
    boundUp <- y.hat + quantileStudentDist * se
    boundLow <- y.hat - quantileStudentDist * se
    return(list(boundUp = drop(boundUp), boundLow = drop(boundLow)))
}
#' 2.5,  Function to Plot the Prediction
plotPredHeating <- function(vec_pred, mat_intervalPred = 0, num_obs = 482, str = "Model"){
    plot(dat.f$series[450: 486], dat.f$heating[450: 486], type = "l", col = "blue", lwd = 2, lty = 1, cex = 0.8, 
         bty = "n", main = paste("Prediction of Heating Power using", str), xlab = "Series", ylab = "Heating Power (W)")
    points(dat.f$series[(num_obs+1): (num_obs+4)], dat.f$heating[(num_obs+1): (num_obs+4)], 
           col = "blue", lty = 1, pch = 4, cex = 1.2)
    points(dat.f$series[(num_obs+1): (num_obs+4)], vec_pred, 
           col = "red", lty = 1, pch = 16, cex = 1.2)
    if (mat_intervalPred != 0){
        for (i in 1: 4){
            lines(c(dat.f$series[(num_obs+i)], dat.f$series[(num_obs+i)]), mat_intervalPred[, i], 
                  col = "red")  # , pch = 18, type = "b"
        }
    }
    legend("bottomleft", inset = .02, legend = c("Training Data", "Testing Data", "Prediction", "Pred Interval"), 
           col = c("blue", "blue", "red", "red"), pch = c(NaN, 4, 16, NaN), lty = c(1, 1, NaN, 1), lwd = c(2, 2, NaN, 1))
}
