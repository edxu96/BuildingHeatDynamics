# Function for Kalman Filter
# Edward J. Xu
# May 16th, 2019
plotTrainTestData <- function(vecX_all, vecY_all, numTrain, vecY_test, strNameData, strPosition = "topleft"){
    numObs <- length(vecY_all)
    plot(vecX_all, vecY_all, type = "l", col = "blue", lwd = 1, lty = 1, cex = 0.8, bty = "n", 
         main = paste("Training and Testing Data of", strNameData), 
         xlab = "Series", ylab = strNameData)
    points(seq(numTrain + 1, numObs), vecY_test, col = "blue", lty = 1, pch = 16, cex = 0.8)
    legend(strPosition, inset = .02, legend = c("Training Data", "Testing Data"), col = c("blue", "blue"), 
           pch = c(NaN, 16), lty = c(1, 1), lwd = c(2, 2))
}
## --------------------------------------------------------------------------------------------------------------
calKalmanFilter <- function(par){
    matSigma1 <- matrix(c(par[5], 0, 0, par[6]), nrow = 2)
    matSigma2 <- matrix(par[7])
    result <- fkf(a0 = c(par[1], par[2]), P0 = diag(c(par[3], par[4]), 2), dt = matB %*% matU, ct = 0, 
                  Tt = matA, Zt = matC, HHt = matSigma1, GGt = matSigma2, yt = matrix(dat$tempIn[1:numTrain], nrow = 1),
                  check.input = TRUE)
    return(result)
}
logLikKalmanFilter <- function(par){
    matSigma1 <- matrix(c(par[5], 0, 0, par[6]), nrow = 2)
    matSigma2 <- matrix(par[7])
    result <- fkf(a0 = c(par[1], par[2]), P0 = diag(c(par[3], par[4]), 2), dt = matB %*% matU, ct = 0, 
                  Tt = matA, Zt = matC, HHt = matSigma1, GGt = matSigma2, yt = matrix(dat$tempIn[1:numTrain], nrow = 1),
                  check.input = TRUE)
    return(-result$logLik)
}
predictKalmanFilter <- function(matA, matB, matU.pred, vecX.filt_latest, matVar.filt_latest, matSigma1, numX = 2){
    numU <- dim(matU.pred)[1]
    numPred <- dim(matU.pred)[2]
    matX.pred <- matrix(NA, nrow = numX, ncol = numPred)
    mat3Var.pred <- array(NA, c(numX, numX, numPred))
    #
    vecColX.filt_latest <- matrix(vecX.filt_latest, nrow = numX)
    vecColX.pred <- matrix(matX.pred[,1], nrow = numX)
    vecColU.pred <- matrix(matU.pred[,1], nrow = numU)
    vecColX.pred <- matA %*% vecColX.filt_latest + matB %*% vecColU.pred
    mat3Var.pred[,,1] <- matA %*% matVar.filt_latest %*% t(matA) + matSigma1
    matX.pred[,1] <- vecColX.pred
    if (numPred >= 2) {
        for (i in 2:numPred) {
            vecColX.pred_latest <- vecColX.pred
            vecColX.pred <- matrix(matX.pred[,i], nrow = numX)
            vecColU.pred <- matrix(matU.pred[,i], nrow = numU)
            vecColX.pred <- matA %*% vecColX.pred_latest + matB %*% vecColU.pred
            mat3Var.pred[,,i] <- matA %*% mat3Var.pred[,,(i - 1)] %*% t(matA) + matSigma1
            matX.pred[,i] <- vecColX.pred
        }
    }
    return(list("matX.pred" = matX.pred, "mat3Var.pred" = mat3Var.pred))
}
predictPlotKalmanFilter <- function(mod, matSigma1, numPlot, matA, matB, matU.pred){
    resultPred <- predictKalmanFilter(matA, matB, matU.pred, mod$att[numTrain], mod$Ptt[,,537], matSigma1)
    matX.pred <- resultPred$matX.pred
    mat3Var.pred <- resultPred$mat3Var.pred
    print(resultPred$matX.pred)
    print(mat3Var.pred[1,1,])
    print(mat3Var.pred[2,2,])
    plotKalmanFilter.pred(numPlot, 3, numTrain, mod$att[1,], dat$tempIn, mod$Ptt[1,1,], matX.pred[1,], 
                          tail(dat$tempIn, n = 3), mat3Var.pred[1,1,])
}
plotRecons <- function(numPlot, numTrain, vecX.recons_all, vecX.meas_all, vecSError_all){
    #
    seqPosition <- seq((numTrain - numPlot + 1), numTrain)
    vecX.recons <- tail(vecX.recons_all, n = numPlot)
    vecX.meas <- tail(vecX.meas_all, n = numPlot + 3)[1:numPlot]
    vecSError <- tail(vecSError_all, n = numPlot)
    # 
    vecLow <- vecX.recons - 1.96 * vecSError 
    vecUp <- vecX.recons + 1.96 * vecSError
    minY <- min(vecX.meas, vecX.recons, vecLow)
    maxY <- max(vecX.meas, vecX.recons, vecUp)
    plot(seqPosition, vecX.meas, type = "l", col = "blue", lty = 2, lwd = 2, bty = "n", xaxt = "n",
         xlab = "series", ylab = "Celsius Degree", ylim = c(minY, maxY))
    title(main = "Reconstruction of Indoor Temperature using Kalman Filter")
    legend("topleft", inset = .02, legend = c("Filtered Data", "Measured Data", "95% Confi-Interval"), 
           col = c("red", "blue", "red"), lty = c(1, 2, 3), lwd = c(2, 2, 1))
    axis(1, at = seqPosition, las = 0, outer = FALSE)
    # Plot Filtered State Variables
    lines(seqPosition, vecX.recons, col = "red", lty = 1, lwd = 2)
    lines(seqPosition, vecLow, col = "red", lty = 3, lwd = 1)
    lines(seqPosition, vecUp, col = "red", lty = 3, lwd = 1)
}
plotKalmanFilter.pred <- function(numPlot, numPred, numTrain, vecX.recons_all, vecX.meas_all, vecSError_all,
                                  vecX.pred, vecX.meas.pred, vecSError.pred){
    ## 0, Select Plotted Data
    seqPosition <- seq((numTrain - numPlot + 1), numTrain + numPred)
    vecX.recons <- tail(vecX.recons_all, n = numPlot)
    vecX.meas <- tail(vecX.meas_all, n = numPlot + 3)[1:numPlot]
    vecSError <- tail(vecSError_all, n = numPlot)
    seqPosition_filt <- seqPosition[1:numPlot]
    seqPosition_pred <- tail(seqPosition, n = numPred)
    ## 1, Plot Measured Data
    vecLow <- vecX.recons - 1.96 * vecSError 
    vecUp <- vecX.recons + 1.96 * vecSError
    vecLow.pred <- vecX.pred - 1.96 * vecSError.pred  # Prediction Interval
    vecUp.pred <- vecX.pred + 1.96 * vecSError.pred  # Prediction Interval
    minY <- min(vecX.meas, vecX.recons, vecLow, vecLow.pred, vecX.pred)
    maxY <- max(vecX.meas, vecX.recons, vecUp, vecUp.pred, vecX.pred)
    plot(seqPosition, c(vecX.meas, vecX.meas.pred), type = "l", col = "blue", lty = 2, lwd = 2, bty = "n", 
         xaxt = "n", xlab = "series", ylab = "Celsius Degree", xlim = c(seqPosition[1], tail(seqPosition, n = 1)), 
         ylim = c(minY, maxY))
    title(main = "Reconstruction and Prediction of Indoor Temperature using Kalman Filter")
    legend("topleft", inset = .02, legend = c("Filtered Data", "Measured Data", "95% Confi-Interval", "Prediction"), 
           col = c("red", "blue", "red", "red"), lty = c(1, 2, 3, 1), lwd = c(2, 2, 1, 2), pch = c(NA, NA, NA, 18))
    axis(1, at = seqPosition, las = 0, outer = FALSE)
    ## 2, Plot Filtered State Variables
    lines(seqPosition_filt, vecX.recons, col = "red", lty = 1, lwd = 2)
    lines(seqPosition_filt, vecLow, col = "red", lty = 3, lwd = 1)
    lines(seqPosition_filt, vecUp, col = "red", lty = 3, lwd = 1)
    ## 3, Plot Prediction Mean and Confidence Interval
    points(seqPosition_pred, vecX.pred, cex = 2, col = "red", pch = 18)
    # points(seqPosition.pred, vecX.meas.pred, pch = 4, cex = 1.5, col = "blue")
    for (i in 1:length(vecX.pred)) {
        lines(rep(seqPosition_pred[i], 2), c(vecLow.pred[i], vecUp.pred[i]), col = "red", lwd = 2)
    }
}