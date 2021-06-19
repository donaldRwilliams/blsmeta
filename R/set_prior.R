#' @title Assign Prior Distributions to Parameters
#'
#' @param param character string. Which parameter (e.g., \code{"(Intercept)"})? 
#'        This \strong{must} match the column names of the model matrix.
#' 
#' @param prior character string. The desired prior distribution, following 
#'              the JAGS formulation with the precision and not variance for 
#'              the scale. See \strong{Details}.
#' 
#' @param dpar character string. Which distributional parameter? The options
#'             include \code{location} (effect-size) and \code{scale} 
#'             (variance component).
#' 
#' @param level character string. The level when \code{dpar = "scale"}. For example,
#'              in a three-level model, set \code{level = "two"} for the level two 
#'              scale model.
#'
#' @return A list that is used internally.
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
#' priors <- make_prior(yi = yi, 
#'                      vi = vi, 
#'                      prior = prior,
#'                      es_id = es_id,
#'                      study_id = study_id,
#'                      data = gnambs2020)
#'                      
#' priors
assign_prior <- function(param, prior, dpar, level = NULL){
  ls <- list(list(param = param, 
                  prior = prior, 
                  level = level))
  names(ls) <- dpar
  ls
}