#' @title Get Prior from Fitted Model
#' 
#' @description This function returns the prior distributions, and
#' can be used to check that the priors are correctly specified
#' (e.g., when supplying custom priors).
#' 
#' @param object An object of class \code{blsmeta}.
#'
#' @return model priors are printed out.
#' 
#' @export
#'
#' @examples
#' library(psymetadata)
#' 
#' fit <-  blsmeta(yi = yi, 
#'                 vi = vi, 
#'                 mods = ~n,
#'                 es_id = es_id,
#'                 data = gnambs2020)
#'                 
#' get_prior(object)
get_prior <- function(object){
  cat(object$prior)
}