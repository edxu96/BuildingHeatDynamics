
setEPS()
postscript("Image/1.eps", width = 11, height = 6)
plotTrainTestData(dat$series, dat$tempIn, numTrain, tail(dat$tempIn, n = 3), "Indoor Temperature")
dev.off()

setEPS()
postscript("Image/2.eps", width = 11, height = 6)
plotTrainTestData(dat$series, dat$tempAir, numTrain, tail(dat$tempAir, n = 3), "Outdoor Temperature")
dev.off()

setEPS()
postscript("Image/3.eps", width = 11, height = 6)
plotTrainTestData(dat$series, dat$solar, numTrain, tail(dat$solar, n = 3), "Solar Irradiation")
dev.off()

setEPS()
postscript("Image/4.eps", width = 11, height = 6)
plotTrainTestData(dat$series, dat$heat, numTrain, tail(dat$heat, n = 3), "Heating Power")
dev.off()

setEPS()
postscript("Image/5.eps", width = 11, height = 6)
plotRecons(100, 537, modKalman_1$att[1,], dat$tempIn, modKalman_1$Ptt[1,1,])
dev.off()

setEPS()
postscript("Image/6.eps", width = 11, height = 6)
predictPlotKalmanFilter(modKalman_1, matSigma1_1, 10, matA, matB, matU.pred)
dev.off()

setEPS()
postscript("Image/7.eps", width = 11, height = 6)
predictPlotKalmanFilter(modKalman_optim, matSigma1_optim, 10, matA, matB, matU.pred)
dev.off()

setEPS()
postscript("Image/8.eps", width = 11, height = 6)
plotRecons(537, 537, modKalman_1$att[1,], dat$tempIn, modKalman_1$Ptt[1,1,])
dev.off()