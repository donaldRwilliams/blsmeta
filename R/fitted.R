#' Title
#'
#' @param re_formula 
#' @param object 
#'
#' @return
#' @export
#'
#' @examples
fitted.blsmeta <- function(object, re_formula = NULL){
  
  # check class
  if(!is(x, "blsmeta")){
    stop("invalid class. must be 'blsmeta'")
  }
  
  samps <- extract_samples(object)
  
  if(is.null(re_formula)){
    
    if(isFALSE(object$save_ranef)){
      stop("save_ranef = FALSE. set to TRUE")
    }
    
    re_2 <- extract_re_2(samps)
    
    betas <- extract_beta(samps, object$mean_X)
    
    k <- ncol(re_2)
    
    yhat <- 
      sapply(1:k, function(x){
        object$xold[x,] %*% t(cbind(betas[,1] + re_2[,x], betas[,2]))
      })
  } else { 
    stop("invalid 'type'. must be 'metafor' or 'brms'")
  } else if (is.na(re_formula)) {
    
    yhat <- object$xold %*% t(extract_beta(samps, object$mean_X))
  }
 return(yhat)
}

