#' Title
#'
#' @param x 
#' @param summary 
#' @param cred 
#'
#' @return
#' @export
#'
#' @examples
total_variability <- function(x, measure = "tau2", summary = TRUE, 
                           cred = 0.95){
  
  # check class
  if(!is(x, "blsmeta")){
    stop("invalid class. must be 'blsmeta'")
  }
  
  tau2 <- tau(x, summary = FALSE)^2
  
  k <- ncol(tau2)
  
  vi <- x$model$data()$v
  
  total_var <- sapply(1:k, function(x) {
    total_var <- tau2[,x] + vi[x]
    return(total_var)
  })
  
  if(measure == "tau"){
    total_var <- sqrt(total_var)
  }
  
  if (summary) {
    creds <- cred_helper(cred)
    lb <- creds[1]
    ub <- creds[2]
    
    total_var <- data.frame(
      Post.mean = colMeans(total_var),
      Post.sd = apply(total_var, 2, sd),
      Cred.lb = apply(total_var, 2,  quantile, lb),
      Cred.ub = apply(total_var, 2,  quantile, ub)
    )
  }
  
  
  
  return(total_var)
}