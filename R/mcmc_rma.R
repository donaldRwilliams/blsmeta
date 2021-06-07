#' @title MCMC Sampling for \code{rma} Objects
#' 
#' @description The package **metafor** is perhaps the gold-standard for 
#'              meta-analysis in \code{R}. This function allows for sampling 
#'              from the posterior distribution of a model originally estimated
#'              with **metafor**.
#'
#' @param object an object of class \code{rma}.
#' 
#' @param data data frame containing the variables in the model.
#' 
#' @param ... Currently ignored
#'
#' @return An object of class \code{blsmeta} (see \code{\link[blsmeta]{blsmeta}})
#' 
#' @note This function works for any kind of model fitted with \code{rma}.
#' 
#' @export
#'
#' @examples
#' library(metafor)
#' library(psymetadata)
#' 
#' fit <- mcmc_rma(rma(yi = yi, vi = vi, 
#'                 method = "FE",
#'                 data = gnambs2020), 
#'                 data = gnambs2020)
#'
#' fit
mcmc_rma <- function(object, data, ...){
  
  if (object$method == "FE") {
    
    dat <- data.frame(yi = object$yi,
                      vi = object$vi)
    
    if (is.null(object$formula.mods)) {
      fit <- blsmeta(yi, vi, data = data)
    } else {
      fit <- blsmeta(yi, vi, 
                     mods = object$formula.mods,  
                     data = data)
    }
  } else {
    dat <- cbind.data.frame(yi = object$yi,
                            vi = object$vi,
                            es_id = 1:object$k,
                            data)
    
    if (class(object)[1] == "rma.uni") {
      if (is.null(object$formula.mods)) {
        fit <- blsmeta(yi, vi, 
                       es_id = es_id, 
                       data = dat)
      } else {
        fit <- blsmeta(
          yi,
          vi,
          mods = object$formula.mods,
          es_id = es_id,
          data = dat
        )
      }
      
    } else if (class(object)[1] == "rma.ls") {
      if (is.null(object$formula.mods)) {
        fit <- blsmeta(
          yi,
          vi,
          es_id = es_id,
          mods_scale2 = object$formula.scale,
          data = dat
        )
      } else {
        fit <- blsmeta(
          yi,
          vi,
          mods = object$formula.mods,
          mods_scale2 = object$formula.scale,
          es_id = es_id,
          data = dat
        )
      }
    } else {
      stop("class not supported. must be 'rma.uni or 'rma.ls'")
    }
    
  }
  return(fit)
}