
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blsmeta: Bayesian Location-Scale Meta-Analysis

[![Build
Status](https://www.travis-ci.com/donaldRwilliams/blsmeta.svg?branch=main)](https://travis-ci.com/donaldRwilliams/blsmeta)

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

-   [Installing JAGS](#installing-jags)
-   [Packages](#packages)
-   [Fixed-Effects Model](#fixed-effects-model)
    -   [Overall Effect](#overall-effect)
    -   [Moderator](#moderator)
-   [Two-Level Model](#two-level-model)
    -   [Overall Effect](#overall-effect-1)
    -   [Scale Moderator](#scale-moderator)
        -   [Predicted Values](#predicted-values)
-   [Three-Level Model](#three-level-model)
-   [MCMC metafor](#mcmc-metafor)

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

More examples coming soon :-)

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
