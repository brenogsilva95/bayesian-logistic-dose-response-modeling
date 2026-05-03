# bayesian-logistic-dose-response-modeling
Bayesian logistic regression for dose-response modeling with LC50 estimation using MCMC (HMC/NUTS).

Paper: https://doi.org/10.4025/actascibiolsci.v43i1.57781

# Bayesian Logistic Regression for Dose-Response Modeling

## Introduction

Pollinating insects, particularly bees, play a crucial role in ecosystem stability and agricultural productivity. However, the increasing use of agrochemicals has raised concerns about their toxicological effects on bee populations.

From a quantitative perspective, understanding how mortality responds to increasing exposure levels is a classical problem in statistics, known as **dose-response modeling**.

In this context, estimating the **lethal concentration (LC50)**, the concentration at which 50% of individuals die, is a key objective. Traditional approaches rely on frequentist logistic or probit regression models. However, these methods may be limited when dealing with small samples or when prior knowledge is relevant.

This project presents a **Bayesian logistic regression framework** for dose-response analysis, providing a probabilistic interpretation of model parameters and enabling more flexible inference.

---

## Experimental Data

The dataset consists of controlled laboratory experiments with *Scaptotrigona bipunctata* bees exposed to different concentrations of the insecticide Fastac Duo.

Key characteristics:

- Binary response variable:
  - $Y_i = 1$ (death)
  - $Y_i = 0$ (alive)

- Predictor:
  - Log-transformed concentration of insecticide

- Three independent experiments:
  - 24h exposure
  - 48h exposure
  - 72h exposure

Each experiment follows a completely randomized design with replicates.

---

## Statistical Model

### Binary Response Model

Let $Y_i$ be a binary random variable:

$$
Y_i \mid \mathbf{x}_i \sim \text{Bernoulli}(p_i), \quad i = 1, \dots, n
$$

where:

$$
p_i = \mathbb{E}[Y_i \mid \mathbf{x}_i]
$$

---

### Logistic Regression

The probability of mortality is modeled using the logistic function:

$$
p_i = \frac{\exp(\boldsymbol{\beta}^\top \mathbf{x}_i)}{1 + \exp(\boldsymbol{\beta}^\top \mathbf{x}_i)}
$$

or equivalently:

$$
\log\left(\frac{p_i}{1 - p_i}\right) = \boldsymbol{\beta}^\top \mathbf{x}_i
$$

where:

- $\boldsymbol{\beta} = (\beta_0, \beta_1, \dots, \beta_k)^\top$ are regression parameters  
- $\mathbf{x}_i$ is the vector of explanatory variables  

This formulation ensures that $p_i \in (0,1)$ and captures the characteristic sigmoidal behavior of dose-response curves.

---

### Likelihood Function

The likelihood is given by:

$$
L(\boldsymbol{\beta}) = \prod_{i=1}^{n} p_i^{y_i} (1 - p_i)^{1 - y_i}
$$

---

## Bayesian Framework

In the Bayesian approach, parameters are treated as random variables.

### Prior Distribution

Non-informative priors were assigned:

$$
\beta_l \sim \mathcal{N}(0, 10^6), \quad l = 0, 1, \dots, k
$$

---

### Posterior Distribution

Using Bayes' theorem:

$$
\pi(\boldsymbol{\beta} \mid \mathbf{Y}, \mathbf{X}) \propto L(\boldsymbol{\beta}) \cdot \pi(\boldsymbol{\beta})
$$

Since the posterior does not have a closed form, numerical methods are required.

---

### MCMC Estimation

Posterior samples were obtained using **Hamiltonian Monte Carlo (HMC)**, specifically the **No-U-Turn Sampler (NUTS)**.

Key aspects:

- Multiple chains
- Burn-in period
- Thinning to reduce autocorrelation
- Convergence diagnostics:
  - $\hat{R} < 1.1$
  - Effective Sample Size (ESS)

This approach provides efficient exploration of high-dimensional posterior distributions.

---

## Interpretation of the Model

### Odds Interpretation

The odds of mortality are defined as:

$$
\text{Odds}_i = \frac{p_i}{1 - p_i} = \exp(\boldsymbol{\beta}^\top \mathbf{x}_i)
$$

An increase in concentration leads to an exponential increase in mortality odds.

---

### LC50 Estimation

The LC50 is obtained by solving:

$$
p = 0.5
$$

which implies:

$$
\boldsymbol{\beta}^\top \mathbf{x} = 0
$$

Thus, the LC50 can be derived from the fitted logistic model.

---

## Results (Statistical Perspective)

- The parameter associated with log-concentration is positive and statistically significant in all experiments  
- Posterior distributions exhibit approximately normal behavior  
- Convergence diagnostics indicate stable MCMC chains ($\hat{R} \approx 1$)  
- Estimated LC50 values increase with exposure time  

As reported in the study :contentReference[oaicite:1]{index=1}:

- 24h: $LC50 \approx 0.03$ g a.i. L$^{-1}$  
- 48h: $LC50 \approx 0.04$ g a.i. L$^{-1}$  
- 72h: $LC50 \approx 0.06$ g a.i. L$^{-1}$  

These results indicate that longer exposure requires higher concentrations to reach the same mortality level.

---

## Model Diagnostics

Model adequacy was assessed using:

- Pearson residuals:

$$
r_i = \frac{y_i - \hat{p}_i}{\sqrt{\text{Var}(\hat{p}_i)}}
$$

- Q-Q plots  
- Residual vs fitted analysis  

Residuals were mostly within the interval $[-3, 3]$, indicating no major violations of model assumptions.

---

## Conclusion

This work demonstrates that Bayesian logistic regression provides a robust framework for dose-response modeling.

Key advantages include:

- Probabilistic interpretation of parameters  
- Incorporation of prior knowledge  
- Improved inference under uncertainty  
- Flexibility in complex experimental settings  

The methodology can be extended to other biological and toxicological studies involving binary outcomes and nonlinear relationships.

---

## Reference

Silva, B. G., Santos, P. R., Lobos, C. M. V., Diniz, T. O., Pereira, N. C., & Ruvolo-Takasusuki, M. C. C. (2021).  
Analysis of a dose-response assay in *Scaptotrigona bipunctata* bees using a Bayesian logistic regression model.  
Acta Scientiarum. Biological Sciences.  
https://doi.org/10.4025/actascibiolsci.v43i1.57781
