
# TidyCrossec: TidyVerse-Style Likelihood-based Modelling Tools for Cross Sectional Data

> Cross-sectional data, or a cross section of a study population, in statistics and econometrics is a type of data collected by observing many subjects (such as individuals, firms, countries, or regions) at the one point or period of time. The analysis might also have no regard to differences in time. [1]

> Cross-sectional data differs from time series data, in which the same small-scale or aggregate entity is observed at various points in time. Another type of data, panel data (or longitudinal data), combines both cross-sectional and time series data ideas and looks at how the subjects (firms, individuals, etc.) change over a time series. [1]

[1]: https://en.wikipedia.org/wiki/Cross-sectional_data

## Obs-Set, Obs-Group, Obs-Point

Observations can be seen as outcomes of random variables.

## Model Parameter Estimation

The likelihood function has a product structure due to the independence assumption, while those with dependent data have other types of structures.

## Work Flow

To find the likelihood function of a model for a set of data can be treated as an optimization problem. The sub- likelihood function is found for each independent sample, then they are combined to form the likelihood function.

`iid observation tibble`

`sub likelihood function tibble`

`likelihood function`

`likelihood results tibble`

Also, log-likelihoods can be used.

`sub log-likelihood function tibble`

`log-likelihood function`

`log-likelihood results tibble`

## Examples in Other Materials

- https://adv-r.hadley.nz/function-factories.html#MLE
- Use nonlinear optimization to compute the maximum likelihood estimate (MLE) of
the parameters of a normal distribution aka the sample mean and variance, https://github.com/JuliaOpt/JuMP.jl/blob/master/examples/mle.jl
- Distribution fitting, https://juliastats.org/Distributions.jl/stable/fit/#Maximum-Likelihood-Estimation-1

## Nomenclature

- `lk`: likelihood
- `lkf`: likelihood function
- `llk`: log-likelihood
- `llkf`: log-likelihood function
- `ci`: confidence interval
  * `lkci`: likelihood-based confidence interval
  * `mleci`: MLE-based confidence interval

## Penal Analysis

The estimator and its estimates have the same notation $\hat{\theta}$.
