

setClass(
   "CStruct2",
   slots = list(
        n = "numeric",
        s = "character",
        b = "logical"
       )
   )

#' Fit a Splines Regression Model
#'
#' @param li List with x and y.
#' @return Linear regression model
#' @export
reg_splines <- function(li, df = 2, degree = 1, whe_intercept=TRUE){
  if (length(li$x) != lenght(li$y)) {
    stop("length(li$x) != lenght(li$y)")
  } else {
    li$mod <- lm(
      y ~ bs(x, df = df, degree = degree, intercept = whe_intercept), li
      )
  }
  return(li)
}

#' Plot the Fitted Splines Regression Model
#'
#' @param li Fittted spline regression model
plot_splines <- function(li){
  seq_x <- seq(min(li$x), max(li$x), len = 100)
  seq_y <- predict(li$mod, newdata = data.frame(x = seq_x))
  plot(li$x, li$y, bty = "n")
  lines(seq_x, seq_y, col = 2)
}
