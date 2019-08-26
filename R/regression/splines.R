
#' Fit a Base Splines Regression Model
#'
reg_bsplines <- function(datf, df, whe_intercept=TRUE){
    if (whe_intercept) {
        lm <- lm(P ~ 0 + bs(Te, df=4, intercept=TRUE), df_x)
    }
    else {
        lm <- lm(P ~ bs(Te, df = df), datf)
    }
    seq_x <- seq(min(df_x$Te), max(df_x$Te), len = 100)
    seq_y <- predict(lm, newdata = data.frame(Te = seq_x))
    plot(df_x$Te, df_x$P)
    ## We need to use predict to see the estimated function
    lines(seq_x, seq_y, col = 2)
    return(lm)
}

plot_bs <- function(x, x_bs){
    ## Merge them with x
    df_x <- data.frame(x, x_bs)
    names(df_x) <- c("x", paste0("bs", 1: (ncol(df_x) - 1)))
    ## Plot
    icol <- grep("^bs", names(df_x))
    plot(df_x$x, df_x$bs1, type="n", ylim=range(df_x[ ,icol]))
    for(i in icol){
        lines(df_x$x, df_x[ ,i], col = i)
    }
}

#' Regression using Base Splines
reg_bs <- function(datf, df){
  lm <- lm(P ~ bs(Te, df = df), datf)

  seq_x <- seq(min(df_x$Te), max(df_x$Te), len = 100)
  seq_y <- predict(fit, newdata = data.frame(Te = seq_x))
  plot(df_x$Te, df_x$P)
  ## We need to use predict to see the estimated function
  lines(seq_x, seq_y)
  return(lm)
}
