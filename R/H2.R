#' Title
#'
#' @param x 
#' @param newdata 
#' @param v 
#' @param summary 
#' @param cred 
#'
#' @return
#' @export
#'
#' @examples
bayes_H2 <- function(x, 
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
  h2 <- apply(level_2_var, 1, function(x){ 
    return((x + s2) / s2)
  })
  
  if (summary) {
    creds <- cred_helper(cred)
    h2 <- data.frame(
      Post.mean = colMeans(h2),
      Post.sd = apply(h2, 2, sd),
      Cred.lb = apply(h2, 2,  quantile, creds[1]),
      Cred.ub = apply(h2, 2,  quantile, creds[2])
    )
  }
  return(h2)   
}