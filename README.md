<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidynamics <a href='https://edxu96.github.io'><img src='./images/1.jpg' align="right" height="138.5" /></a>

<!-- badges: start -->
Tidy Analysis of Multivariate (Non)-Linear Dynamic Systems
<!-- badges: end -->

## Introduction

For those who are not statisticians, like economists and traders, a well-established collections of statistical tools is needed, and this package is my toolbox to model dynamics in renewable energy systems and markets. You can also try to model other systems.

> R packages are (after a short learning phase) a comfortable way to maintain collections of R functions and data sets. As an article distributes scientific ideas to others, a package distributes statistical methodology to others. Most users first see the packages of functions distributed with R or from CRAN. The package system allows many more people to contribute to R while still enforcing some standards. But packages are also a convenient way to maintain private functions and share them with your colleagues. I have a private package of utility function, my working group has several “experimental” packages where we try out new things. This provides a transparent way of sharing code with co-workers, and the final transition from “playground” to production code is much easier. (["Creating R Packages: A Tutorial", Friedrich Leisch](https://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf))

Modern statistics in R.

> The tidyverse is an opinionated collection of R packages designed for data science. All packages share an underlying design philosophy, grammar, and data structures. ([tidyverse](https://www.tidyverse.org))

### Features

- [x] `tidyverse` Favored Grammar
- [ ] Adaptivity
- [ ] Based on Likelihood Theory
- [ ] Matrix Computation
- [x] Functionals and Structs

## How to Install

```
install.packages(pkgs = tidynamics, repos = https://github.com/edxu96/tidynamics.git)
```

```
> devtools::install_github("edxu96/tidynamics")
```

The input and output from observations of some dynamic process can always be combined to a matrix, which we call `mat_oi`. For outputs with uni-variate time series, there are only two columns in the matrix. For those with multivariate time series, it's convenient to see the data in matrix.

## How to Use

The following categories of models will be included in this package.

| Method                  | Static / Dynamic | Linear / Non-Linear |
| ----------------------- | ---------------- | ------------------- |
| Linear Regression       | Static           | Linear              |
| Linear Additive Decomp. | Static           | Linear              |
| Generalized Additive M. | Static           | Non-Linear          |
| ARIMA (without input)   | Dynamic          | Linear              |
| Input-Output Model      | Dynamic          | Linear              |
| Linear State Space M.   | Dynamic          | Linear              |
| Stochastic Diff. Eq.    | Dynamic          | Non-Linear          |
| Tree-Based M.           | Static           | Non-Linear          |

### Vignettes

- [x] [Linear Regression](./files/1-linear.pdf)
- [ ] [Grey-Box Modeling](./files/2-greybox.pdf)
- [ ] [Non-Linear Regression](./files/3-pred.pdf)
- [ ] [Model Predictive Control](./files/4-mpc/pdf)

## When to Use

Well-Defined Data from Physical Systems

* Prediction -> Supervised Learning, High Accuracy
    - Stochastic Programming
    - Dynamic Programming
    - Model Predictive Control
* Inference -> Supervised Learning, High Interpretability
    - Science Research
* Summarize -> Unsupervised Learning
    - Visualization
    - Group

## More Info

- Detailed explaination can be found in [edxu96/tidynamics/wiki](https://github.com/edxu96/tidynamics/wiki/1-Home).
- 极简动力：多元非线性动态系统的极简风格分析。
