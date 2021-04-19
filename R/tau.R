#' Title
#'
#' @param x 
#' @param summary 
#' @param cred 
#' @param mod_tau_2 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
tau <- function(x, mod_tau_2 = NULL, data, summary = TRUE, cred = 0.95){
  # check class
  if(!is(x, "blsmeta")){
    stop("invalid class. must be 'blsmeta'")
  }
  
  # extract samples
  samps <- extract_samples(x)
  
  # level 2 scale coefficients
  gammas <- extract_gamma(x = samps, mean_X2 = x$mean_X2)
  
  if (is.null(mod_tau_2)) {
    # fitted values
    level_2_sd <- t(exp(x$x2old %*%  t(gammas)))
  } else {
    level_2_sd <- t(exp(model.matrix(mod_tau_2, data) %*% t(gammas)))
  }
  
  if (summary) {
    
    creds <- cred_helper(cred)
    
    lb <- creds[1]
    ub <- creds[2]
    
    level_2_sd <- data.frame(
      Post.mean = colMeans(level_2_sd),
      Post.sd = apply(level_2_sd, 2, sd),
      Cred.lb = apply(level_2_sd, 2,  quantile, lb),
      Cred.ub = apply(level_2_sd, 2,  quantile, ub)
    )
  }
  return(level_2_sd)   
}