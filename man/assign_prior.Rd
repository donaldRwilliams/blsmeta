% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_prior.R
\name{assign_prior}
\alias{assign_prior}
\title{Assign Prior Distributions to Parameters}
\usage{
assign_prior(param, prior, dpar, level = NULL)
}
\arguments{
\item{param}{character string. Which parameter (e.g., \code{"(Intercept)"})?
This \strong{must} match the column names of the model matrix.}

\item{prior}{character string. The desired prior distribution, following
the JAGS formulation with the precision and not variance for
the scale. See \strong{Details}.}

\item{dpar}{character string. Which distributional parameter? The options
include \code{location} (effect-size) and \code{scale}
(variance component).}

\item{level}{character string. The level when \code{dpar = "scale"}. For example,
in a three-level model, set \code{level = "two"} for the level two
scale model.}
}
\value{
A list that is used internally.
}
\description{
Assign Prior Distributions to Parameters
}
\examples{
library(psymetadata)

prior <- c(assign_prior(param = "(Intercept)", 
           prior = "dnorm(0, 1)", dpar = "location"),
           assign_prior(param = "(Intercept)", 
           prior = "dnorm(-2, 1)", 
           dpar = "scale", level = "two")
           )
priors <- make_prior(yi = yi, 
                     vi = vi, 
                     prior = prior,
                     es_id = es_id,
                     study_id = study_id,
                     data = gnambs2020)
                     
priors
}
