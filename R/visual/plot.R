
#' Plot the data points from training and testing sets
#'
#' @export
plot_traintest <- function(x_train, y_train, x_test, y_test, str_title){
  plot(
    c(x_train, x_test), c(y_train, y_test), type = "b", col = "blue", lwd = 2,
    lty = 1, cex = 0.8, main = str_title
  )
  points(x_test, y_test, col = "blue", lty = 1, pch = 16, cex = 0.8)
  legend(
    "topleft", inset = .02, legend = c("Training Data", "Testing Data"),
    col = c("blue", "blue"), pch = c(1, 16), lty = c(1, 1), lwd = c(2, 2)
  )
}
