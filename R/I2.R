#' Title
#'
#' @param x 
#' @param newdata 
#' @param summary 
#'
#' @return
#' @export
#'
#' @examples
I2 <- function(x, newdata = NULL, v = NULL, summary){
  
  if(!is(x, "blsmeta")){
    stop("invalid class. must be 'blsmeta'")
  }
  
  samps <- extract_samples(x)
  
  if(is.null(v)){
    s2 <- s2_helper(x$model$data()$v)
    } else {
      
      s2 <- v
  }
  
  gammas <- extract_gamma(samps, x$mean_X2)
  
  if(is.null(newdata)){
    
    level_2_var <- exp(x$x2old %*%  t(gammas))^2
    
  } else{
    
    newdata <- as.matrix(cbind.data.frame("(Intercept)" =  1, newdata))
    
    if(any(isFALSE( colnames(x$x2old) == colnames(newdata)))){
      message("invalid newdata. must match old data, including column names and order")
    }
    
    level_2_var <- newdata %*% t(gammas)
  }
  
  icc <- apply(
    level_2_var, 1, function(x){ 
      x / (x + s2)
    }
  )
  return(icc)   
}