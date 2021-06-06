#' Title
#'
#' @param object 
#' @param data 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
mcmc_rma <- function(object, data, ...){
  
  if (object$method == "FE") {
    dat <- data.frame(yi = object$yi,
                      vi = object$vi)
    
    if (is.null(object$formula.mods)) {
      fit <- blsmeta(yi, vi, data = data)
    } else {
      fit <- blsmeta(yi, vi, mods = object$formula.mods,  data = data)
    }
  } else {
    dat <- cbind.data.frame(yi = object$yi,
                            vi = object$vi,
                            es_id = 1:object$k,
                            data)
    
    if (class(object)[1] == "rma.uni") {
      if (is.null(object$formula.mods)) {
        fit <- blsmeta(yi, vi, es_id = es_id, data = dat)
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
          scale = object$formula.scale,
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