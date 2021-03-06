
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/blsmeta_hex.png" width = 250 />

# blsmeta: Bayesian Location-Scale Meta-Analysis

[![Build
Status](https://www.travis-ci.com/donaldRwilliams/blsmeta.svg?branch=main)](https://travis-ci.com/donaldRwilliams/blsmeta)

The goal of **blsmeta** is to provide a user-friendly interface for
Bayesian meta-analysis, including fixed-effects, two-level, and
three-level (for dependent effect sizes) random-effects models.

Additionally, a key feature of **blsmeta** is “scale” modeling, which
allows for predicting the variance components with moderators (e.g.,
perhaps between-study variance is not constant across studies). As a
result, heterogeneity statistics and prediction intervals are then a
function of those same moderators, thereby opening the door to **better
understanding heterogeneity in meta-analysis**.

Version 1 is forthcoming, say, by the end of June 2021.

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("donaldRwilliams/blsmeta")
```

Note that the development version will be fully usable, in so far as it
will have been tested, documented, etc. It will gradually include more
functions over the coming weeks (merged from other branches),
culminating in the official release.

Below, there are some simple examples demonstrating how to use
**blsmeta**. In the future, there will be several examples showcasing
the utility of scale modeling in meta-analysis.

## Table of Contents

-   [Installing JAGS](#installing-jags)
-   [Fixed-Effects Model](#fixed-effects-model)
    -   [Overall Effect](#overall-effect)
    -   [Moderator](#moderator)
-   [Two-Level Model](#two-level-model)
    -   [Overall Effect](#overall-effect-1)
    -   [Scale Moderator](#scale-moderator)
        -   [Predicted Values](#predicted-values)
-   [Three-Level Model](#three-level-model)
    -   [Comparing Variance Components](#comparing-variance-components)
-   [MCMC metafor](#mcmc-metafor)
-   [Custom Priors](#user-defined-priors)
-   [Student Led Projects](#forthcoming-features)
    -   [Diversity](#diversity)

## Installing JAGS

**blsmeta** uses the popular Bayesian software JAGS to estimate the
models. It must be downloaded from the following link:
<https://sourceforge.net/projects/mcmc-jags/files/>

## Packages

    # install for data
    if (!require('psymetadata')){
      install.packages('psymetadata')
      }
    library(psymetadata)
    library(blsmeta)

## Fixed-Effects Model

### Overall Effect

    # fit model
    fit_fe <- blsmeta(yi = yi, vi = vi, 
                      data = gnambs2020)
                      
    # results
    fit_fe

    #> Model: Fixed-Effects
    #> Studies: 67 
    #> Samples: 20000 (4 chains)
    #> Formula: ~ 1 
    #> ------
    #> Location:
    #>             Post.mean Post.sd Cred.lb Cred.ub Rhat
    #> (Intercept)     -0.07    0.03   -0.12   -0.02 1.00
    #>
    #> ------
    #> Date: Mon Jun 07 12:03:56 2021 

There is an important difference from the **metafor** package, where, by
default, a random-effects model is fitted. This is not the case in
**blsmeta**, where, by default, a fixed-effects model will be estimated
if the level two variable is not provided.

### Moderator

    fit_fe <- blsmeta(yi = yi, vi = vi, 
                      mods = ~ 0 + color,
                      data = gnambs2020)

    # results
    fit_fe

    #> Model: Fixed-Effects
    #> Studies: 67 
    #> Samples: 20000 (4 chains)
    #> Formula: ~ 0 + color 
    #> ------
    #> Location:
    #>            Post.mean Post.sd Cred.lb Cred.ub Rhat
    #> colorblack     -0.04    0.13   -0.30    0.22 1.00
    #> colorblue      -0.04    0.07   -0.18    0.10 1.00
    #> colorgray      -0.12    0.05   -0.22   -0.01 1.00
    #> colorgreen     -0.06    0.03   -0.13    0.00 1.00
    #> colorwhite      0.00    0.12   -0.23    0.22 1.00

    #> ------
    #> Date: Mon Jun 07 12:21:07 2021 

In the not too distant future (this was written on 6/7/21), it will be
possible to compare those effects (e.g., `colorgreen - colorwhite`).

## Two-Level Model

## Overall Effect

A two-level random-effects meta-analysis is implemented with

    fit_re <- blsmeta(yi = yi, vi = vi, 
                      es_id = es_id,
                      data = gnambs2020)

    # results
    fit_re

    #> Model: Two-Level
    #> Studies: 67 
    #> Samples: 20000 (4 chains)
    #> Location Formula: ~ 1 
    #> Scale Formula: ~ 1 
    #> Note: 'Scale' on standard deviation scale
    #> ------
    #> Scale:
    #>               Post.mean Post.sd Cred.lb Cred.ub Rhat
    #> sd(Intercept)      0.10    0.06    0.02    0.22 1.00
    #> 
    #> Location:
    #>             Post.mean Post.sd Cred.lb Cred.ub Rhat
    #> (Intercept)     -0.08    0.03   -0.14   -0.02 1.00
    #>
    #> ------
    #> Date: Mon Jun 07 12:26:24 2021 

Notice the argument `es_id`, which corresponds to the effect size id
(`1:k`, where `k` is the number of studies).

## Scale Moderator

A key feature of **blsmeta** is scale modeling that allows for
predicting the between-study variance (or “scale”) with moderators (just
like for the effect size or “location”). In this following example,
heterogeneity is predicted study size.

    fit_re <- blsmeta(yi = yi, vi = vi, 
                      es_id = es_id,
                      mods_scale2 = ~ n, 
                      data = gnambs2020)

    # results
    fit_re

    #> Model: Two-Level
    #> Studies: 67 
    #> Samples: 20000 (4 chains)
    #> Location Formula: ~ 1 
    #> Scale Formula: ~ n 
    #> Note: 'Scale' on standard deviation scale
    #> ------
    #> Scale:
    #>             Post.mean Post.sd Cred.lb Cred.ub Rhat
    #> (Intercept)      0.01    0.42   -0.77    0.88 1.03
    #> n               -0.03    0.01   -0.05   -0.01 1.03
    #>
    #> Location:
    #>             Post.mean Post.sd Cred.lb Cred.ub Rhat
    #> (Intercept)     -0.06    0.03   -0.11    0.00 1.00
    #>
    #> ------
    #> Date: Mon Jun 07 12:30:18 2021 

Notice that the `n` parameter is negative, implying that studies with
larger sample sizes are more consistent (i.e., less heterogeneity). That
effect is on the log-scale, which is far from intuitive.

### Predicted Values

To make sense of the scale model, it is possible to obtain predicted
values of between-study heterogeneity at particular values of the
moderator, that is,

    tau2(fit_re, type = "sd", 
         newdata_scale2 = data.frame(n = seq(20, 200, 20)))

    #>    Post.mean Post.sd Cred.lb Cred.ub
    #> 1      0.634   0.165   0.357   1.003
    #> 2      0.378   0.083   0.226   0.553
    #> 3      0.234   0.072   0.096   0.372
    #> 4      0.149   0.063   0.037   0.273
    #> 5      0.098   0.053   0.014   0.207
    #> 6      0.065   0.043   0.005   0.161
    #> 7      0.045   0.035   0.002   0.126
    #> 8      0.031   0.028   0.001   0.100
    #> 9      0.022   0.022   0.000   0.080
    #> 10     0.015   0.018   0.000   0.063

Notice `type = "sd"`, which ensures we are on the standard deviation
scale (easier to interpret). The results indicate that there is quite a
bit of heterogeneity in small studies, but it goes to practically zero
as study size increases.

## Three-Level Model

Three-level location-scale meta-analysis is fully implemented as well.
The key is providing the `study_id` argument, which is the higher level
grouping variable that the effect sizes are nested within. This
accommodates dependent effect sizes.

    fit <-  blsmeta(yi = yi, 
                    vi = vi, 
                    es_id = es_id,
                    study_id = study_id,
                    data = gnambs2020)
                  
    fit

    #> Model: Three-Level
    #> Studies2: 67 
    #> Studies3: 22 
    #> Samples: 20000 (4 chains)
    #> Location Formula: ~ 1 
    #> Scale2 Formula: ~ 1 
    #> Scale3 Formula: ~ 1 
    #> Note: 'Scale' on standard deviation scale
    #> ------
    #> Scale2:
    #>               Post.mean Post.sd Cred.lb Cred.ub Rhat
    #> sd(Intercept)      0.06    0.04    0.01    0.15 1.00
    #> 
    #> Scale3:
    #>               Post.mean Post.sd Cred.lb Cred.ub Rhat
    #> sd(Intercept)      0.20    0.06    0.09    0.34 1.01
    #> 
    #> Location:
    #>             Post.mean Post.sd Cred.lb Cred.ub Rhat
    #> (Intercept)     -0.12    0.06   -0.25   -0.02 1.00
    #> 
    #> ------
    #> Date: Sun Jun 13 11:27:08 2021 

### Comparing Variance Components

One question might be whether one variance component is larger, which
can be tested with the `linear_hypothesis` function.

    linear_hypothesis(obj = fit, 
                      cred = 0.90,
                      lin_comb = "scale3_Intercept > scale2_Intercept",
                      sub_model = "scale")
                      
    #> Hypotheses:
    #>  C1: scale3_Intercept > scale2_Intercept 
    #> ------ 
    #> Posterior Summary:
    #> 
    #>    Post.mean Post.sd Cred.lb Cred.ub Pr.less Pr.greater
    #> C1      1.27    0.73    0.14    2.51    0.03       0.97
    #> ------ 
    #> Note:
    #> Pr.less: Posterior probability less than zero
    #> Pr.greater: Posterior probability greater than zero

These estimates are on the log-scale, and there is a 0.97 posterior
probability that the level three variance component is larger than the
level two variance component.

In the future, it will be possible to compare these models with the
Bayes factor.

## MCMC metafor

The package **metafor** is perhaps the gold-standard for meta-analysis
in `R`. In **blsmeta**, it is possible to sample from the posterior
distribution of a model originally estimated with **metafor** (`rma`
objects are currently supported).

    library(metafor)

    fit <- mcmc_rma(rma(yi = yi, vi = vi, 
                     method = "FE", data = gnambs2020), 
                     data = gnambs2020)
    # results
    fit

    #> Model: Fixed-Effects
    #> Studies: 67 
    #> Samples: 20000 (4 chains)
    #> Formula: ~ 1 
    #> ------
    #> Location:
    #>             Post.mean Post.sd Cred.lb Cred.ub Rhat
    #> (Intercept)     -0.07    0.03   -0.12   -0.02 1.00
    #>
    #> ------
    #> Date: Mon Jun 07 13:27:13 2021 

This function works for any kind of model fitted with `rma`.

## User-Defined Priors

User-defined priors can be defined for each parameter. This is
accomplished with the `assign_prior` function. For example,

    prior <- 
      c(assign_prior(param = "(Intercept)", 
                    prior = "dnorm(0, pow(1, -2))", 
                    dpar = "location"),
        assign_prior(param = "n", 
                     prior = "dnorm(0, 1)", 
                     dpar = "location"),
        assign_prior(param = "(Intercept)", 
                  prior = "dnorm(-0.5, pow(1.5, -2))", 
                  dpar = "scale", level = "two")
        )

The `pow(1.5, -2)` allows for specifying the standard deviation, whereas
`JAGS` uses the precision, or the inverse of the variance, which can be
confusing (hence use `pow(., -2)`). This would then be used in the
`prior` argument of `blsmeta`.

For a sanity check, it is possible to verify that the priors made it to
the correct parameters as follows

    priors <- make_prior(yi = yi, 
               vi = vi, 
               mods = ~ n,
               prior = prior,
               es_id = es_id,
               study_id = study_id,
               data = gnambs2020)

    priors

    #> #location priors
    #> 
    #> #(Intercept)
    #> beta[1] ~ dnorm(0, pow(1, -2))
    #> 
    #> #n
    #> beta[2] ~ dnorm(0, 1)
    #> 
    #> #scale level two priors
    #> 
    #> #(Intercept)
    #> gamma[1] ~ dnorm(-0.5, pow(1.5, -2))
    #> 
    #> #scale level three priors
    #> #Intercept
    #> eta[1] ~ dnorm(-2, 1)

Notice that the “scale” priors are negative. At first, this may not seem
correct because the scale refers to the variance. By default, however, a
log-linear model is fitted to the variance components. As a result, the
priors are on the log-scale which is very flexible on the one hand, but
on the other, not very intuitive.

To better understand those priors, use `sample_prior`.

    samps <- sample_prior(priors, iter = 50000)

Then you can plot the prior with
`hist(exp(samps$gamma), xlim = c(0, 2), breaks = 10000)`.

## Forthcoming Features

There are a variety of things strategically left out of **blsmeta**.
This is because I am hoping to be a professor. To this end, I am
planning to tackle the following with students that join my lab:

1.  Bayesian hypothesis testing (with the Bayes factor)

2.  Visualization (with **ggplot2**)

3.  Shiny Application

4.  Website

Option 1 could be a first year project for a graduate student. I have
several other ideas for meta-analysis (to help get the ball rolling, if
interested), but these would not likely be a part of **blsmeta**. Option
2 will result in authorship on the software paper for **blsmeta**
(written once students contribute). Options 3-4 will be ongoing for
undergraduate students. For each option, students will learn valuable
skills for industry (e.g., data science) and academia (e.g., pursing a
PhD).

Note also this “lab” exists only in thought, and will hopefully come to
fruition in the fall of 2022 or 2023.

### Diversity

While I intend to save these projects, working with underrepresented
students (BIPOC, first-generation, students from developing countries,
etc.) takes precedence. If you are interested in the above options, or
have an idea of your own, please email (<drwwilliams@ucdavis.edu>) or DM
on Twitter (<https://twitter.com/wdonald_1985>). I prefer Twitter.

Women of color and Native Americans are especially encouraged to reach
out.
