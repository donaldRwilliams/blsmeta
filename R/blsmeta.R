#' Title
#'
#' @param yi 
#' @param vi 
#' @param sei 
#' @param es_id 
#' @param study_id 
#' @param mods 
#' @param mods_scale2 
#' @param mods_scale3 
#' @param prior 
#' @param iter 
#' @param warmup 
#' @param chains 
#' @param log_linear 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
blsmeta <- function(yi, vi, 
                    sei, 
                    es_id, 
                    study_id, 
                    mods = ~ 1, 
                    mods_scale2 = ~ 1, 
                    mods_scale3 = ~ 1, 
                    prior = NULL,
                    iter = 5000, 
                    warmup = 1000, 
                    chains = 4, 
                    log_linear = TRUE,
                    data) {
  
  args <- match.call()
  
  k <- nrow(data)
  
  # fixed effects model
  if (is.null(args$es_id)) {
    if (is.null(args$mods)) {
      X_location <- model.matrix( ~ 1, data)
      X_location_old <- X_location
      X_location_means <- 1
      
    } else {
      X_location <- model.matrix(mods, data)
      
      if (all(X_location[, 1] == 1)) {
        center_X <- center_helper(X_location)
        X_location <- center_X$x
        X_location_old <- center_X$xold
        X_location_means <- center_X$x_mean
        
      } else {
        X_location_old <- X_location
        X_location_means <- NULL
      }
    }
    
    prior_location <- lapply(which(names(prior) == "location"), 
                             function(x){ prior[[x]] })
    
    dat_list <- data_helper(data = data, arg = args)
    
    prior <- location_prior(prior_location,
                   X_location = X_location,
                   yi = dat_list$y)
    
    model_code <- paste0("model{\n", fe_rjags,
                         "\n", "#location priors\n",
                          prior, 
                         "\n}")
    
    design_mats <- list(X_location = X_location)
    
    dat_list_jags <- c(dat_list , design_mats, K = k)
    
    params <- c("beta")

    message("blsmeta: Adaption")
    suppressWarnings({
      fit <- jags.model(file = textConnection(model_code),
                      data = dat_list_jags,
                      n.chains = chains, 
                      quiet = TRUE)
      })
    
    message("blsmeta: Posterior Sampling")
    
    samps <- coda.samples(fit,
                          variable.names = params,
                          n.iter = iter + warmup)
    
    message("blsmeta: Finished")
    
    returned_object <- list(posterior_samples = samps, 
                            X_location = X_location,  
                            X_location_old = X_location_old, 
                            X_location_means = X_location_means,
                            args  = args, 
                            chains =  chains,
                            iter = iter,
                            warmup = warmup,
                            prior = prior,
                            model = "fe", 
                            mods_f = mods) 
  } else {
    # two level
    if(is.null(args$study_id)){
      if(is.null(args$mods)){
        X_location <- model.matrix(~ 1, data)
        X_location_old <- X_location
        X_location_means <- 1
      } else {
        X_location <- model.matrix(mods, data)
        if(all(X_location[, 1] == 1)){
          center_X <- center_helper(X_location)
          X_location <- center_X$x
          X_location_old <- center_X$xold
          X_location_means <- center_X$x_mean
        } else {
          X_location_old <- X_location
          X_location_means <- NULL
        }
      }
      
      if(is.null(args$mods_scale2)){
        X_scale2 <- model.matrix(~ 1, data)
        X_scale2_old <- X_scale2
        X_scale2_means <- 1
      } else {
        X_scale2 <- model.matrix(mods_scale2, data)
        if(all(X_scale2[, 1] == 1)){
          center_X <- center_helper(X_scale2)
          X_scale2 <- center_X$x
          X_scale2_old <- center_X$xold
          X_scale2_means <- center_X$x_mean
        } else {
          X_scale2_old <- X_scale2
          X_scale2_means <- NULL
        }
      }
      
      prior_location <- lapply(which(names(prior) == "location"), 
                               function(x){ prior[[x]] })
      
      prior_scale2 <- lapply(which(names(prior) == "scale"), 
                               function(x){ prior[[x]] })
      
      dat_list <- data_helper(data = data, arg = args)
      
      design_mats <- list(X_location = X_location, 
                          X_scale_2 = X_scale2)
      
      dat_list_jags <- c(dat_list , design_mats, K = k)
      
      prior_location <- location_prior(prior_location,
                                       X_location = X_location,
                                       yi = dat_list$y)
      
      prior_scale2 <- scale_level_two_prior(prior_scale2,
                                            X_scale_2 = X_scale2)
      
      
      priors <- paste0("#location priors\n", 
                       prior_location, 
                       "\n\n#scale level two priors\n",  
                       prior_scale2)
      
      model_code <- paste0("model{\n", two_level_rjags,
                           "\n", 
                           priors, 
                           "\n}")
      
      
      params <- c("beta", "gamma", "re_2", "tau_2")
      
      message("blsmeta: Adaption")
      suppressWarnings({
        fit <- jags.model(file = textConnection(model_code),
                          data = dat_list_jags,
                          n.chains = chains, 
                          quiet = TRUE)
      })
      
      message("blsmeta: Posterior Sampling")
      
      samps <- coda.samples(fit,
                            variable.names = params,
                            n.iter = iter + warmup)
      
      message("blsmeta: Finished")
      
      returned_object <- list(posterior_samples = samps, 
                              X_location = X_location,  
                              X_location_old = X_location_old, 
                              X_location_means = X_location_means,
                              X_scale2 = X_scale2,  
                              X_scale2_old = X_scale2_old, 
                              X_scale2_means = X_scale2_means,
                              args  = args, 
                              chains =  chains,
                              iter = iter,
                              warmup = warmup,
                              prior = prior,
                              model = "two_level", 
                              mods_f = mods, 
                              mods_scale2_f = mods_scale2) 
      
      
      
      
      
      
      
      
      
      
      
      
      # three level
    } else {
      
      
      
      
    }
    
  }
  class(returned_object) <- "blsmeta"
  return(returned_object)
}