# MatrixTSA/examples/reg_trend: Steady State Building Model
# Edward J. Xu
# Aug 26th, 2019

rm(list = ls())
library(MatrixTSA)
library(magrittr)

get_datf <- function(str_file){
  datf <- read.csv(
    str_file, sep = ",", header = T,
    col.names = c("time", "h", "t_e", "i")
  )
  datf$series <- seq(1, length(datf$time))
  return(datf)
}

plot_datf <- function(datf, num, str_title){
  plot(
    datf$s, datf$h, type = "b", col = "blue", lwd = 2, lty = 1,
    ylab = "Heating (W)", xlab = "Series", cex = 0.8, main = str_title
  )
  points(
    datf$s[(num + 1):(num + 8)], datf$h[(num + 1):(num + 8)],
    col = "blue", lty = 1, pch = 16, cex = 0.8
  )
  legend(
    "topleft", inset = .02, legend = c("Training Data", "Testing Data"),
    col = c("blue", "blue"), pch = c(1, 16), lty = c(1, 1), lwd = c(2, 2)
  )
}

datf_3h <- get_datf("./data/house/3h.csv")
n_3h <- length(datf_3h$series) - 8
plot_datf(datf_3h, n_3h, "Training and Testing Heating Data Sampled per 3h")

datf_6h <- get_datf("./data/house/6h.csv")
n_6h <- length(datf_6h$series) - 4
plot_datf(datf_6h, n_6h, "Training and Testing Heating Data Sampled per 6h")

