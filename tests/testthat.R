# Test
# Edward J. Xu
# Aug 26, 2019

library(testthat)
library(MatrixTSA)

source("./tests/testthat/greybox.R")

context("Grey-Box Modelling")

test_main <- function(){
  test_that("Test test_convert_expr", {
    expr_new <- test_convert_expr()
    expect_equal(expr_new, dTi ~ 1 / Ci * Ph * dt + exp(p11) * dw1)
  })

  test_that("Test set_mod_ctsm", {
    str_result <- test_set_mod_ctsm()
    expect_equal(str_result, "Pass")
  })
}

test_main()
