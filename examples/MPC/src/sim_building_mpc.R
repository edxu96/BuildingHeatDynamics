
library(scales)
library(lpSolve)

sim_building_mpc <- function(li_mat_ss, li_mat_ss_d, ti){

  # Change from air heating to floor heating
  altB <- li_mat_ss$b
  altB[1, 2] <- 0
  altB[2, 2] <- 1 / fit1$xm[5]
  altBd <- solve(li_mat_ss$a) %*% (li_mat_ss_d$a - diag(dim(li_mat_ss$a)[1])) %*% altB

  ti$Ph1

  diff_t <- diff(ti$t)[1] #all.equal(diff(diff(data$t)),rep(0,length(data$t)-2)) ## Make sure that time steps are uniform

  set.seed(133)

  Controllable <- c(FALSE, TRUE, FALSE) # The controllable inputs e.g. heating in this example, but not outside air temperature or solar radiation
  MaxHeat <- 1.5 # Maximum amount of heating
  N <- 60 # Control horizon
  Ymax <- 25 # Maximum temperature
  Ymin <- 22.5 # Minimum temperature
  Air <- TRUE # air heating or floor heating?
  Noise <- 0

  Stochastic <- FALSE # Stochastic boundaries?
  ViolationFraction <- 0.05 # How many temperature violations?
  Tmax <- length(ti$timedate) - N
  # Price <- rep(1,Tmax+N) # Should the prices be constant?
  Price <- prbs(6, 322, 10) + 1 # Or varying?
  ForecastedPrice <- Price

  if (Air) {
    cBd <- li_mat_ss_d$b[, Controllable, drop = FALSE]
    ncBd <- li_mat_ss_d$b[, !Controllable, drop = FALSE]
  } else {
    cBd <- altBd[, Controllable, drop = FALSE]
    ncBd <- altBd[, !Controllable, drop = FALSE]
  }

  # Penalty <- read.csv("Penalty60min.csv",header = FALSE)$V1[1:Tmax]
  # Penalty <- approx(Penalty,n=Tmax+24,method='constant')$y
  #
  # Spot <- arima.sim(model=list(ar=c(0.8),ma=c(0.7,0.5)),n=Tmax)+15
  # Spot <- approx(Spot,n=Tmax+24,method='constant')$y
  #
  # Penalty <- Penalty*15
  # PenSpot <- Spot + Penalty

  Phie <- NULL
  Phix <- NULL
  Gu <- NULL
  Gd <- NULL
  for (i in 1:N) {
    Phix <- rbind(Phix, li_mat_ss$c %*% (li_mat_ss_d$a %^% i))
    # Phie <- rbind(Phie, li_mat_ss$c %*% (li_mat_ss_d$a %^% (i-1)) %*% K)

    gu <- NULL
    gd <- NULL
    for (j in i:(-N + i + 1)) {
      if (j > 0) {
        gu <- cbind(gu, li_mat_ss$c %*% (li_mat_ss_d$a %^% (j - 1)) %*% cBd)
        gd <- cbind(gd, li_mat_ss$c %*% (li_mat_ss_d$a %^% (j - 1)) %*% ncBd)
      } else {
        gu <- cbind(gu, li_mat_ss$c %*% (li_mat_ss_d$a %*% cBd * 0))
        gd <- cbind(gd, li_mat_ss$c %*% (li_mat_ss_d$a %*% ncBd * 0))
      }
    }
    Gu <- rbind(Gu, gu)
    Gd <- rbind(Gd, gd)
  }


  Ainq <- cbind(rbind(-Gu, Gu, matrix(0, nrow = dim(Gu)[1], ncol = dim(Gu)[2])), rbind(-diag(dim(Gu)[1]), -diag(dim(Gu)[1]), -diag(dim(Gu)[1])))
  dir <- rep("<=", N * 3)
  types <- rep("li_mat_ss$c", N * 2)


  ### Comment out if using RGLP
  Ainq <- rbind(Ainq, cbind(diag(N), matrix(0, N, N)))

  Bounds <- list(lower = list(ind = seq(1L, 2 * N), val = numeric(2 * N)), upper = list(ind = seq(1L, 2 * N), val = c(rep(MaxHeat, N), rep(Inf, N))))


  Sigma <- Noise * matrix(c(
    exp(fit1$xm[8]) / fit1$xm[4] * sqrt(diff_t), 0,
    0, exp(fit1$xm[9]) / fit1$xm[5] * sqrt(diff_t)
  ), byrow = TRUE, nrow = 2)^2

  Uncertainty <- sqrt(rowSums(apply(Phix %*% Sigma, 2, cumsum)))


  ControlBuilding <- function(Price, e, T, Tmax, Tmin, Stochastic = 0) {
    Penalty <- 1E+12 # Penalty for violating temperature constraints
    N <- length(Price) # Predictin Horizon
    f <- t(c(Price, rep(Penalty, N)))

    binq <- matrix(c(-Tmin + qnorm(ViolationFraction) * Stochastic * Uncertainty + Phix %*% T + Gd %*% e, Tmax - qnorm(ViolationFraction) * Stochastic * Uncertainty - Phix %*% T - Gd %*% e, numeric(N), rep(MaxHeat, N)), ncol = 1)
    # Rglpk_solve_LP(f,Ainq,dir,binq,bounds=Bounds,types=types) # Change the remaining lines for this one if using LGPK solver
    lp(objective.in = f, const.mat = Ainq, const.dir = "<=", const.rhs = binq)
  }

  ### Simulate brownian motion

  ### Simulate measurement errors
  Bin <- cumsum(rnorm(Tmax, sd = exp(fit1$xm[8]) / fit1$xm[4] * sqrt(diff_t)))
  Bm <- cumsum(rnorm(Tmax, sd = exp(fit1$xm[9]) / fit1$xm[5] * sqrt(diff_t)))

  BM <- Noise * rbind(Bin, Bm)

  ### Simulate the system
  Tall <- matrix(0, nrow = 2, ncol = Tmax)
  u <- numeric(Tmax)


  Tall[, 1] <- c(23, 23)

  for (i in 2:Tmax) {
    u[i] <- ControlBuilding(c(Price[i], ForecastedPrice[(i + 1):(i + N - 1)]), c(rbind(ti$Ta[i:(i + N - 1)], ti$Gv[i:(i + N - 1)])), Tall[, (i - 1)], Ymax, Ymin, Stochastic)$solution[1]
    Tall[, i] <- li_mat_ss_d$a %*% Tall[, (i - 1)] + 1 * cBd %*% u[i] + ncBd %*% c(ti$Ta[i], ti$Gv[i]) + t(diff(t(BM)))[, (i - 1), drop = FALSE]
  }

  return(
    list(
      Tall = Tall,
      Ymin = Ymin,
      Ymax = Ymax,
      Tmax = Tmax,
      Price = Price,
      u = u
    )
  )
}
