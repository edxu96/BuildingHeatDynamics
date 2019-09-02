# TidyDynamics: Multivariate Regression of System Dynamics in Struct

![](./images/tangram_1.png)

## Introduction

For those who are not statisticians, like economists and traders, a well-established collections of statistical tools is needed, and this package is my toolbox to model dynamics in renewable energy systems and markets.

> R packages are (after a short learning phase) a comfortable way to maintain collections of R functions and data sets. As an article distributes scientific ideas to others, a package distributes statistical methodology to others. Most users first see the packages of functions distributed with R or from CRAN. The package system allows many more people to contribute to R while still enforcing some standards. But packages are also a convenient way to maintain private functions and share them with your colleagues. I have a private package of utility function, my working group has several “experimental” packages where we try out new things. This provides a transparent way of sharing code with co-workers, and the final transition from “playground” to production code is much easier. (["Creating R Packages: A Tutorial", Friedrich Leisch](https://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf))

## How to Use

```
install.packages(pkgs = MatrixTSA, repos = https://github.com/edxu96/MatrixTSA.git)
```

```
> devtools::install_github("edxu96/MatrixTSA")
```

The input and output from observations of some dynamic process can always be combined to a matrix, which we call `mat_oi`. For outputs with uni-variate time series, there are only two columns in the matrix. For those with multivariate time series, it's convenient to see the data in matrix.

```
mat_oi <- get_mat_oi(datf)
```

## Underdevelopment

- [ ] Generalized Additive Model
- [x] Grey-Box Modelling
- [ ] Tree-Based Method
- [ ] Generalized Linear Model

## When to Use

Well-Defined Data from Physical Systems

* Forecast -> Supervised Learning, High Accuracy
    - Stochastic Programming
    - Dynamic Programming
    - Model Predictive Control
* Inference -> Supervised Learning, High Interpretability
    - Science Research
* Summarize -> Unsupervised Learning
    - Visualization
    - Group

## When not to Use

* Classification

## Theory Behind

* Stochastic Differential Equation
* Optimization
* Statistics

## More Info

- Detailed explaination can be found in [edxu96/TSA-Energy/wiki](https://github.com/edxu96/TSA-Energy/wiki/1-Home).
- 矩阵时序分析：通过矩阵来计算多元时序分析。
