#' @title Draw Samples from the Prior Distributions
#' 
#' @description This function is useful for visualizing the 
#' prior distributions.
#'
#' @param object An object of class \code{make_prior}.
#' 
#' @param iter numeric. The number of posterior samples per chain 
#'             (defaults to \code{5000}, excluding \code{warmup}).
#' 
#' @param ... Currently ignored.
#'
#' @return A data frame including the posterior samples.
#' 
#' @details Note that parameters include 
#'          \code{beta} (location or effect size), 
#'          \code{gamma} (level-two scale model),
#'          and \code{eta} (level-three scale model). Importantly,
#'          the scale model priors are on the log-scale (use \code{exp}).
#' 
#' @export
#'
#' @examples
#' library(psymetadata)
#' 
#' prior <- c(assign_prior(param = "(Intercept)", 
#'            prior = "dnorm(0, 1)", dpar = "location"),
#'            assign_prior(param = "(Intercept)", 
#'            prior = "dnorm(-2, 1)", 
#'            dpar = "scale", level = "two")
#'            )
#'
#' priors <- make_prior(yi = yi, 
#'                      vi = vi, 
#'                      prior = prior,
#'                      es_id = es_id,
#'                      study_id = study_id,
#'                      data = gnambs2020)
#' 
#' samps <- sample_prior(priors, iter = 50000)
#' hist(samps$beta)
sample_prior <- function(object, iter = 5000, ...){
  
  if(!is(object, "make_prior")){
    stop("must be of class 'make_prior'")
  }
  
  model_code <- paste0("model{\n", 
                       object$priors, "\n}")
  
  mod <- jags.model(file = textConnection(model_code), 
                    n.chains = 1, quiet = TRUE)
  
  params <- c("beta", "gamma", "eta")
  message("blsmeta: Prior Sampling")
  
  suppressWarnings({
    samps <- coda.samples(model = mod, 
                          variable.names = params, 
                          n.iter = iter)
  })
  
  message("blsmeta: Finished")
  samps <- do.call(rbind.data.frame, samps)
  
  return(samps)
  
}