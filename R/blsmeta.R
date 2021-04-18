#' Title
#'
#' @param yi 
#' @param vi 
#' @param es_id 
#' @param mod 
#' @param mod_tau_2
#' @param iter 
#' @param chains 
#' @param data 
#' @param store_varying 
#'
#' @return
#' @export
#'
#' @examples
blsmeta <- function(yi, vi, 
                    es_id = NULL, 
                    mod = ~ 1, 
                    mod_tau_2 = ~ 1, 
                    store_varying = TRUE,
                    iter = 5000,
                    chains = 4,
                    data){
  
  arg <- match.call()
  
  k <- nrow(data)
  
  if(is.null( arg$es_id ) ){
    arg$es_id <- 1:k
  }
  
  if(!isTRUE(all.equal(es_id, 1:k))) {
    arg$es_id <- 1:k
  }
  
  if( is.null( arg$mod ) ){
    X <- model.matrix(~ 1, data)
  } else {
    X <- model.matrix(mod, data)
  }
  
  if(is.null(arg$mod_tau_2)) {
    X2 <- model.matrix( ~ 1, data)
    x2old <- X2
    mean_X2 <- 1
  } else {
    X2 <- model.matrix(mod_tau_2, data)
    center_X2 <- center_helper(X2)
    X2 <- center_X2$x
    x2old <- center_X2$xold
    mean_X2 <- center_X2$x_mean
  }
  
  dat_list <- data_helper(data = data, arg = arg)
  
  design_mats <- list(X = X, X2 = X2)
  
  dat_list <- c(dat_list, 
                prior_gamma_helper(X2),
                prior_beta_helper(X, mean(dat_list$y)), 
                design_mats, K = k)
  
  if (store_varying) {
    params <- c("gamma", "beta", "re_2")
  } else {
    params <- c("gamma", "beta")
  }
  
  fit <- jags(
    data = dat_list,
    DIC = FALSE,
    n.chains = chains,
    n.iter = iter + 500,
    progress.bar = "text",
    n.burnin = 500,
    model.file = two_level,
    parameters.to.save = params
  )
  
  fit$mean_X2 <- mean_X2
  fit$x2old <- x2old
  fit$arg <- arg
  class(fit) <- c("rjags", 
                  "blsmeta")
  return(fit)
}