
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blsmeta: Bayesian Location-Scale Meta-Analysis

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
**blsmeta**. In the future, there will lots of examples showcasing the
utility of scale modeling in meta-analysis.

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
possible to compare those effects (e.g., `colorgreen - colorwhte`).

## Random-Effects Model

A two level meta-analysis is implemented with
