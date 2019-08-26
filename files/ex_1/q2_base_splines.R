## ----Initialize-----------------------------------------------
setwd("~/GitHub/TSA-Energy")
rm(ls = list())

require(splines)

source("./src/NonPara.R")

## See what it returns for a sequence from -1 to 1
x = seq(-1, 1, len = 1000)
plot(x)
x_bs <- bs(x, intercept = TRUE)
plot(x_bs)
str(x_bs)
## It is actually a matrix with attributes
class(x_bs[ , ])
## Use colnames() for matrix instead of names() for data.frame
colnames(x_bs[ , ])

plot_bs(x, x_bs)

## ----Degrees of freedom-----------------------------------------
## Change degrees of freedom (of the spline function)
## Generate the base splines
x_bs <- bs(x, df = 7, intercept = TRUE)[ , ]
## Plot and add the quantiles
plot_bs(x, x_bs)

## Change degree (of the piece-wise polynomials, i.e. polynomials between the knot points)
## Generate the base splines
x_bs <- bs(x, df = 7, degree = 1, intercept = TRUE)[ , ]
## Plot
plot_bs(x, x_bs)

## ----Specify knot points----------------------------------------
## Or specify the knot points directly
## As the quantiles
df <- 7
knots <- quantile(x, probs = seq(0, 1, by = 1 / (df-1)))
knots

## Generate the base splines of degree = 1 with the knots as the quantiles
##
## Note that the inner knots are given by the "knots" argument, the
##   boundary knots are min(x) and max(x) (see ?bs "Boundary.knots")
x_bs <- bs(x, knots = knots[2:(length(knots)-1)], degree = 1, intercept = TRUE)[ , ]
## Plot
plot_bs(x, x_bs)
abline(v = knots)

## Another degree
## Generate the base splines of degree with the quantile knots
x_bs <- bs(x, knots = knots[2:(length(knots)-1)], degree = 3, intercept = TRUE)[ , ]
## Plot
plot_bs(x, x_bs)
abline(v = knots)

## ----Non-equidistant quantiles--------------------------------
## Another sequence, where the quantiles are not equidistant
x = c(seq(-1,0,len=200), seq(0,1,len=100), seq(1,2,len=500))

## Generate the base splines of degree with the quantile knots
df <- 7 
x_bs <- bs(x, df = df, degree = 1, intercept = TRUE)[ , ]
## Plot
plot_bs(x, x_bs)
## Add the knots
knots <- quantile(x, probs = seq(0, 1, by = 1 / (df - 1)))
abline(v = knots)
