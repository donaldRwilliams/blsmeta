#' Title
#'
#' @param x 
#' @param mod_tau2 
#' @param data 
#' @param summary 
#' @param cred 
#'
#' @return
#' @export
#'
#' @examples
shrinkage_factor <- function(x, 
                             mod_tau2 = NULL, 
                             summary = TRUE, 
                             cred = 0.95, 
                             data){
  
  # extract samples
  samps <- extract_samples(x)
  
  # level 2 scale coefficients
  gammas <- extract_gamma(x = samps, mean_X2 = x$mean_X2)
  
  if (is.null( mod_tau2)) {
    # fitted values
    level_2_var <- t(exp(x$x2old %*%  t(gammas))^2)
    
    vi <- x$model$data()$v
    
    k <- length(vi)
    
    shrinkage_factor <- sapply(1:k, function(x) {
      sf <- level_2_var[,x] /  (level_2_var[,x] + vi[x])
      return(sf)
    })
    
  } else {
    
    if(!any(c("v") %in% colnames(data))){
      stop("invalid data. missing the variance 'v'")
    }
    
    vi <- data$v
    
    k <- length(vi)
    
    level_2_var <- t(exp(model.matrix(mod_tau2, data) %*% t(gammas)))
    
    shrinkage_factor <- sapply(1:k, function(x) {
      sf <- level_2_var[,x] /  (level_2_var[,x] + vi[x])
      return(sf)
    })
    
  }
  
  if (summary) {
    creds <- cred_helper(cred)
    shrinkage_factor <- data.frame(
      Post.mean = colMeans(shrinkage_factor),
      Post.sd = apply(shrinkage_factor, 2, sd),
      Cred.lb = apply(shrinkage_factor, 2,  quantile, creds[1]),
      Cred.ub = apply(shrinkage_factor, 2,  quantile, creds[2])
    )
  }
  
  return(shrinkage_factor)
}
