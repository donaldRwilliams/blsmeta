#' @title Extract Fixed-Effects Estimates
#' 
#' @name fixef
#'      
#' @description Extract the fixed-effects from a \code{blsmeta} object.
#' 
#' @param object An object of class \code{blsmeta}.
#' 
#' @param summary  logical. Should the posterior samples be summarized 
#'                 (defaults to \code{TRUE})?
#' 
#' @param cred numeric. credible interval (defaults to \code{0.95}).
#' 
#' @param digits numeric. The desired number of digits for the summarized 
#'               estimates (defaults to \code{3}).
#' 
#' @param ... Currently ignored.
#'
#' @return A data frame with either the summarized or (un)summarized 
#'         posterior samples.
#' 
#' @note Note these are equivalent to "population-level" effects 
#' (as used in the \code{R} package \strong{brms})
#' 
#' @export
#'
#' @examples
#' # for data
#' library(psymetadata)
#' 
#' # sample posterior
#' fit <-  blsmeta(yi = yi, vi = vi, 
#'                 mods = ~ color,  
#'                 data = gnambs2020)
#' 
#' # fixed effects                 
#' fixef(fit)
fixef.blsmeta <- function(object, 
                          summary = TRUE, 
                          cred = 0.95, 
                          digits = 3, 
                          ...){
  
  fixefs <- .extract_beta(object)
  colnames(fixefs) <- colnames( object$X_location_old )
  
  if(summary){
    fixefs <- .summary_helper(fixefs, cred = cred)
    row.names(fixefs) <- colnames( object$X_location_old )
    fixefs <- round(x = fixefs, 
                    digits = digits)
  }
  
  return(fixefs)
  
}