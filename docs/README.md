
# Docs for Tidylikelihood

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
