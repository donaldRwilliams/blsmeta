#' Title
#'
#' @param x 
#' @param newdata 
#' @param summary 
#' @param v 
#' @param cred 
#'
#' @return
#' @export
#'
#' @examples
bayes_I2 <- function(x, 
                     newdata = NULL, 
                     v = NULL, 
                     summary = FALSE, 
                     cred = 0.95){
  
  # check class
  if(!is(x, "blsmeta")){
    stop("invalid class. must be 'blsmeta'")
  }
  
  # extract samples
  samps <- extract_samples(x)
  
  if(is.null(v)) {
    # compute 'typical' s2
    s2 <- s2_helper(vi = x$model$data()$v, method = "ht")
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
      message("invalid newdata. must match old data, 
              including column names and order")
    }
    # fitted values
    level_2_var <- exp(newdata %*% t(gammas))^2
  }
  
  # compute 'i2'
  i2 <- apply(level_2_var, 1, function(x){ 
    return(x / (x + s2))
    })
  
  if (summary) {
    creds <- cred_helper(cred)
    i2 <- data.frame(
      Post.mean = colMeans(i2),
      Post.sd = apply(i2, 2, sd),
      Cred.lb = apply(i2, 2,  quantile, creds[1]),
      Cred.ub = apply(i2, 2,  quantile, creds[2])
    )
  }
  return(i2)   
}