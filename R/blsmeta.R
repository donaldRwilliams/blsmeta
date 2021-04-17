#' Title
#'
#' @param yi 
#' @param vi 
#' @param es_id 
#' @param mod 
#' @param scale_level_two 
#' @param iter 
#' @param chains 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
blsmeta <- function(yi, vi, 
                    es_id = NULL, 
                    mod, scale_level_two, 
                    iter = 5000,
                    chains = 4,
                    data){
  
  arg <- match.call()
  
  k <- nrow(data)
  
  if(is.null( arg$es_id ) ){
    arg$es_id <- 1:k
  }
  
  if(!isTRUE(all.equal(es_id, 1:k))){
    arg$es_id <- 1:k
  }
  
  if(is.null( arg$mod ) ){
    X <- model.matrix(~ 1, data)
  } else {
    X <- model.matrix(mod, data)
  }
  
  if(is.null(arg$scale_level_two)){
    X2 <- model.matrix(~ 1, data)
    xold <- X2
  } else {
    X2 <- model.matrix(scale_level_two, data)
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
  
  fit <- jags(data = dat_list, 
              DIC = FALSE,
              n.chains = chains, 
              n.iter = iter + 500, 
              progress.bar = "text",
              n.burnin = 500, 
              model.file = two_level,
              parameters.to.save = c("gamma", 
                                     "re_2", 
                                     "beta"))
  
  fit$mean_X2 <- mean_X2
  fit$x2old <- x2old
  class(fit) <- c("rjags", 
                  "blsmeta")
  return(fit)
}