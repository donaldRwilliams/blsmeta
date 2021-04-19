#' Title
#'
#' @param x 
#' @param mod 
#' @param mod_tau_2 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
predict.blsmeta <- function(x, mod, mod_tau_2, data){
  
  
  samps <- extract_samples(x)
  gammas <- extract_gamma(samps, x$mean_X2)
  betas <- extract_beta(samps, x$mean_X)
  
  X <- model.matrix(mod, data)
  X2 <- model.matrix(mod_tau_2, data)
  
  
  pred_beta <- X %*% t(betas)
  pred_tau <- exp(X2 %*% t(gammas))
  
  n_row <- nrow(pred_tau)
  
  pred <- sapply(1:n_row, function(x) {
    
    rnorm(nrow(samps), pred_beta[x,], pred_tau[x,])
    
  })
  return(pred)  
}
