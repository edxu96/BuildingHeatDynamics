# MatrixDynamics: Multivariate Regression of System Dynamics in Matrix

![](./images/tangram_1.png)

## Introduction

__Regression Model__

1. Model
2. State Space Model
    - Non-Linear State Space Model
    - Linear State Space Model
3. Generalized Additive Model
4. Generalized Linear Model
5. Tree-Based Method

__Unsupervised Learning__

Clustering

## How to Use

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
