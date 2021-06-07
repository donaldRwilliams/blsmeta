
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
