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
  
  # check class
  if(!is(x, "blsmeta")){
    stop("invalid class. must be 'blsmeta'")
  }
  # extract samples
  samps <- extract_samples(x)
  
  if(is.null(v)) {
    # compute 'typical' s2
    s2 <- s2_helper(vari = x$model$data()$v)
  } else {
    # user defined sampling variance
    s2 <- v
  }
  
  # level 2 scale coefficients
  gammas <- extract_gamma(x = samps, mean_X2 = x$mean_X2)
  
  if (is.null(newdata)) {
    # fitted values
    level_2_var <- exp(x$x2old %*%  t(gammas)) ^ 2
  } else {
    # user data
    newdata <-
      as.matrix(cbind.data.frame("(Intercept)" =  1, newdata))
    
    # check column names
    if (any(isFALSE(colnames(x$x2old) == colnames(newdata)))) {
      message("invalid newdata. must match old data, including column names and order")
    }
    # fitted values
    level_2_var <- exp(newdata %*% t(gammas))^2
  }
  
  # compute 'icc'
  icc <- apply(level_2_var, 1, function(x){ 
    return(x / (x + s2))
    })
  
  return(icc)   
}