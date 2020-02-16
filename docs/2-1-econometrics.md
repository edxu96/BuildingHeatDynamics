
## L2

1. We view each single observation \(Y_{i}\) as a realization of a r.v. also denoted \(Y_{i} .\) Suppose this r.v. is Bernoulli distributed with success parameter \(\theta .\) State the density of \(Y_{i} .\) Show that \(E\left[Y_{i}\right] = \theta\) and \(\operatorname{Var}\left[Y_{i}\right] = \theta(1-\theta)\)

<p align="center"><img src="/docs/tex/6bb9a36191f24d42b34fbbe21d3fc76f.svg?invert_in_darkmode&sanitize=true" align=middle width=112.28202315pt height=49.315569599999996pt/></p>

<p align="center"><img src="/docs/tex/7096ef3e3a172d4bd7b7a6d94180fbc8.svg?invert_in_darkmode&sanitize=true" align=middle width=57.32199495pt height=16.438356pt/></p>

2. Given data \(Y_{1}, Y_{2}, ., Y_{n},\) write the Bernoulli model as a set of joint data densities, i.e. like our general formulation of the statistical model, \(\mathcal{M}=\left\{\mathrm{f}_{\psi}\left(y_{1}, y_{2}, ., y_{n}\right), \psi \in \Psi\right\}, \Psi \subseteq \mathbb{R}^{k} .\) State a condition that the model is correctly specified. This will be assumed in the following.

3. Show furthermore that these joint data densities (i.e. the elements of \(\mathcal{M}\) ) can be written in terms of the sample mean, \(\bar{y}\) Hintt: use that, for numbers \(a_{i}\) and \(b_{i}\) it holds that, \(\prod_{i=1}^{n} a_{i} b_{i}=\prod_{i=1}^{n} a_{i} \Pi_{i}^{n} \Pi_{i=1}^{n} b_{i}\) Hint2: use eq. 1.3 .1 in Hint 3: use the definition of the sample meanl.

4. State the log-likelihood function for the Bernoulli model and go through the derivations leading to the \(\mathrm{MLE}, \widehat{\theta}\)

5. Show that \(\widehat{\theta}\) is unbiased and that the variance of it is \(\frac{V a r\left[Y_{1}\right]}{n} .\) What is the standard error of \(\widehat{\theta} .\)

6. Consider the distributional statement \(\left.\widehat{\theta} \stackrel{P}{\rightarrow} \theta_{0}, \text { where } \theta_{0} \in\right] 0 ; 1[\) is the population value/true value. Explain what it means and what assumptions are needed.

7. What does it mean that \(\widehat{\theta}\) is asymptotically normally (Gaussian) distributed? Explain why it holds.
