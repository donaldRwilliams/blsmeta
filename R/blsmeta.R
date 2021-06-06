#' @title Bayesian Meta-Analysis via (Mixed-Effects) Location-Scale Models
#' 
#' @description Fit meta-analytic models, including fixed-effects, two-level, 
#'              and three-level random-effects models. Moderators can be 
#'              included for both location and scale parameters.
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
  
  #note: somewhat redundant code
  #(each model is self contained)
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

    message("blsmeta: Adaption (Fixed-Effects)")
    
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
                            mods_f = mods, 
                            dat_list = dat_list) 
  # two level
  } else {
  
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
      
      message("blsmeta: Adaption (Two-Level)")
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
                              prior = priors,
                              model = "two_level", 
                              mods_f = mods, 
                              mods_scale2_f = mods_scale2, 
                              dat_list = dat_list) 
      # three level
    } else {
      
      lvl3 <- as.numeric(eval(args[[match("study_id", names(args))]], 
                              envir = data))
      
      dat3 <- cbind.data.frame(lvl3 = lvl3, data)
      dat3 <- dat3[!duplicated(dat3$lvl3),]
      J <- nrow(dat3)
      
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
      
      if(is.null(args$mods_scale3)){
        X_scale3 <- model.matrix(~ 1, dat3)
        X_scale3_old <- X_scale3
        X_scale3_means <- 1
      } else {
        X_scale3 <- model.matrix(mods_scale3, dat3)
        if(all(X_scale3[, 1] == 1)){
          center_X <- center_helper(X_scale3)
          X_scale3 <- center_X$x
          X_scale3_old <- center_X$xold
          X_scale3_means <- center_X$x_mean
        } else {
          X_scale3_old <- X_scale3
          X_scale3_means <- NULL
        }
      }
      
      
      prior_location <- lapply(which(names(prior) == "location"), 
                               function(x){ prior[[x]] })
      
      prior_scale2 <- lapply(which(names(prior) == "scale"), 
                             function(x){ prior[[x]] })
      
      prior_scale3 <- lapply(which(names(prior) == "scale"), 
                             function(x){ prior[[x]] })
      
      dat_list <- data_helper(data = data, arg = args)

      design_mats <- list(X_location = X_location, 
                          X_scale_2 = X_scale2, 
                          X_scale_3 = X_scale3)
      
      dat_list_jags <- c(dat_list , 
                         design_mats, 
                         K = k, 
                         J = J)
      
      prior_location <- location_prior(prior_location,
                                       X_location = X_location,
                                       yi = dat_list$y)
      
      prior_scale2 <- scale_level_two_prior(prior_scale2,
                                            X_scale_2 = X_scale2)
      
      prior_scale3 <- scale_level_three_prior(prior_scale3,
                                              X_scale_3 = X_scale3)
      
      
      priors <- paste0("#location priors\n", 
                       prior_location, 
                       "\n\n#scale level two priors\n",  
                       prior_scale2, 
                       "\n\n#scale level three priors\n", 
                       prior_scale3 )
      
      model_code <- paste0("model{\n", three_level_rjags,
                           "\n", 
                           priors, 
                           "\n}")
      
      params <- c("beta", "gamma", 
                  "eta", "re_2", 
                  "tau_2", "re_3", 
                  "tau_3")
      
      message("blsmeta: Adaption (Three-Level)")
      
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
                              X_scale3 = X_scale3,  
                              X_scale3_old = X_scale3_old, 
                              X_scale3_means = X_scale3_means,
                              args  = args, 
                              chains =  chains,
                              iter = iter,
                              warmup = warmup,
                              prior = priors,
                              model = "three_level", 
                              mods_f = mods, 
                              mods_scale2_f = mods_scale2, 
                              mods_scale3_f = mods_scale3, 
                              dat_list = dat_list, 
                              model_code = model_code) 
      
      
  }
    
  }
  class(returned_object) <- "blsmeta"
  return(returned_object)
}