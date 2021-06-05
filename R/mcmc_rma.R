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
  # remove
  # object <-  rma(yi, vi, mods = ~ design, method = "FE", data = data)
  if(object$method == "FE"){
    # dat <- data.frame(yi = object$yi, vi = object$vi)
    if(is.null(object$formula.mods)){
      fit <- blsmeta(yi, vi, data = data)
    } else {
      fit <- blsmeta(yi, vi, mods = object$formula.mods,  data = data)
      
    }
  } else {
    
    dat <- data.frame(yi = object$yi, vi = object$vi, es_id = 1:object$k)
    
    if(class(object)[1] == "rma.uni"){
      
    } else if (class(object)[1] == "rma.ls"){
      
    } else {
      stop("class not supported. must be 'rma.uni or 'rma.ls'")
    }
    
  }
  return(fit)
}