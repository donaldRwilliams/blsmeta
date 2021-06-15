#' @title  Extract Posterior Samples
#' 
#' @description  Extract posterior samples from an 
#'               object of class \code{blsmeta}
#' 
#' @param object An object of class \code{blsmeta}.
#' 
#' @param param character string. The parameter(s) to be extracted.
#'              See \strong{Details}
#'              
#' @param print_names logical. Should the parameter names be printed 
#'                    (defaults to \code{FALSE})? This is useful
#'                    for seeing which parameters can be extracted.            
#'
#' @return A \code{iter} * \code{chains}  by parameter matrix of posterior
#'         samples
#' 
#' @details Options for \code{param} depend on the model, but can included
#'          \code{beta} (fixed-effects for location), 
#'          \code{gamma} (level-two log-linear scale model), 
#'          \code{eta} (level-three log-linear scale model), 
#'          \code{tau_2} (level-two variane components),
#'          \code{tau_3} (level-three variance components),
#'          \code{re_2} (level-two random-effects), and
#'          \code{re_3} (level-three random-effects).
#'
#' Note the variance components, and log-linear coefficients are on 
#' the standard deviation scale.                    
#' 
#' @export
#'
#' @examples
#' library(psymetadata)
#' 
#' fit <-  blsmeta(yi = yi, 
#'                 vi = vi, 
#'                 es_id = es_id,
#'                 data = gnambs2020)
#'                 
#' posterior_samples(object = fit, 
#'                   print_names = TRUE)
posterior_samples <- function(object, param = NULL, 
                              print_names  = FALSE){
  
  if(print_names){
    print(colnames(.extract_samples(object)))
  } else {
  samps <- as.matrix(.extract_samples(object))
  
  if(!is.null(param)){
    samps <- as.matrix(samps[, grep("tau", colnames(samps)) ])
  }
  return(samps)

  }
}