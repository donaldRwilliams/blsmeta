#' Title
#'
#' @param re_formula 
#' @param object 
#' @param summary 
#' @param cred 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
fitted.blsmeta <- function(object, 
                           re_formula = NULL, 
                           summary = TRUE, 
                           cred = 0.95, ...){
  
  # check class
  if(!is(object, "blsmeta")){
    stop("invalid class. must be 'blsmeta'")
  }
  
  samps <- extract_samples(object)
  
  if(is.null(re_formula)){
    
    if(isFALSE(object$save_ranef)){
      stop("save_ranef = FALSE. set to TRUE")
    }
    
    re_2 <- extract_re_2(samps)
    
    betas <- extract_beta(samps, object$mean_X)
    
    p_beta <- ncol(betas)
    
    k <- ncol(re_2)
    
    yhat <- 
      sapply(1:k, function(x){
        
        if(p_beta == 1){
          yhat <- object$xold[x,] %*% t(cbind(betas[,1] + re_2[,x]))
        } else {
          yhat <-  object$xold[x,] %*% t(cbind(betas[,1] + re_2[,x], betas[,2:p_beta]))
          }
        return(yhat)
          })
  } else if (is.na(re_formula)) {
    
    yhat <- t(object$xold %*% t(extract_beta(samps, object$mean_X)))
  } else { 
    stop("invalid 'type'. must be 'metafor' or 'brms'")
  } 
  
  if (summary) {
    creds <- cred_helper(cred)
    lb <- creds[1]
    ub <- creds[2]
    
    # yhat <- t(yhat)
    
    yhat <- data.frame(
      Post.mean = colMeans(yhat),
      Post.sd = apply(yhat, 2, sd),
      Cred.lb = apply(yhat, 2,  quantile, lb),
      Cred.ub = apply(yhat, 2,  quantile, ub)
    )
  }
  
 return(yhat)
}

