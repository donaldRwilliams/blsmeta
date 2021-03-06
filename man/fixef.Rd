% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixef_blsmeta.R
\name{fixef}
\alias{fixef}
\alias{fixef.blsmeta}
\title{Extract Fixed-Effects Estimates}
\usage{
\method{fixef}{blsmeta}(object, summary = TRUE, cred = 0.95, digits = 3, ...)
}
\arguments{
\item{object}{An object of class \code{blsmeta}.}

\item{summary}{logical. Should the posterior samples be summarized
(defaults to \code{TRUE})?}

\item{cred}{numeric. credible interval (defaults to \code{0.95}).}

\item{digits}{numeric. The desired number of digits for the summarized
estimates (defaults to \code{3}).}

\item{...}{Currently ignored.}
}
\value{
A data frame with either the summarized or (un)summarized
posterior samples.
}
\description{
Extract the fixed-effects from a \code{blsmeta} object.
}
\note{
Note these are equivalent to "population-level" effects
(as used in the \code{R} package \strong{brms})
}
\examples{
# for data
library(psymetadata)

# sample posterior
fit <-  blsmeta(yi = yi, vi = vi, 
                mods = ~ color,  
                data = gnambs2020)

# fixed effects                 
fixef(fit)
}
