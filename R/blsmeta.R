#' @title Bayesian Meta-Analysis via (Mixed-Effects) Location-Scale Models
#' 
#' @rdname blsmeta 
#'   
#' @description Fit meta-analytic models, including fixed-effects, two-level, 
#'              and three-level random-effects models. Moderators can be 
#'              included for both the location and scale parameters. This 
#'              is accomplished with mixed-effects location-scale modeling 
#'              \insertCite{@see for example @Hedeker2008}{blsmeta}, with the basic 
#'              idea extended to meta-analysis in 
#'              \insertCite{williams2021putting;textual}{blsmeta}.
#' 
#' @param yi vector with the observed effect sizes (length \code{k}).
#' 
#' @param vi vector with the sampling variances (length \code{k}).
#'           Provide either \code{vi} or \code{sei}.
#' 
#' @param sei vector with the sampling variances (length \code{k}).
#'            Provide either \code{vi} or \code{sei}.
#' 
#' @param es_id numeric vector with the effect size ids (i.e., \code{1:k}). When 
#'              provided, a two-level random-effects model is estimated. 
#'              Whereas, when not specified, a fixed-effects model is estimated
#'              by default.
#' 
#' @param study_id numeric vector with the study ids (length \code{k}). When provided, 
#'                 a three-level random-effects model is estimated 
#'                 (\code{es_id} is required). Note that the \code{yi}'s are 
#'                 assumed to be nested within \code{study_id}. 
#'                
#' @param mods an object of class \code{\link[stats]{formula}}, 
#'             including moderator(s) for the effect size. By default, an
#'             intercept only model is fitted (i.e., \code{mods = ~ 1}), 
#'             resulting in the overall effect.
#' 
#' @param mods_scale2 an object of class \code{\link[stats]{formula}}, 
#'             including moderator(s) for level two variance component 
#'             (or "scale"). By default, an intercept only model is fitted 
#'             (i.e., \code{mods = ~ 1}), resulting in the customary estimate 
#'             (assumed constant across studies).
#' 
#' @param mods_scale3 an object of class \code{\link[stats]{formula}}, 
#'             including moderator(s) for level three variance component 
#'             (or "scale"). By default, an intercept only model is fitted 
#'             (i.e., \code{mods = ~ 1}), resulting in the customary estimate 
#'             (assumed constant across studies). See \strong{Details}.
#' 
#' @param prior one or more \code{blsmetaprior} objects created by 
#'              \code{set_prior}.
#' 
#' @param iter numeric. The number of posterior samples per chain 
#'             (defaults to \code{5000}, excluding \code{warmup}).
#' 
#' @param warmup numeric. The number of warmup samples, which are discarded 
#'               (defaults to \code{1000}).
#' 
#' @param chains numeric. The number of chains (defaults to \code{4})
#' 
#' @param log_linear logical. Should the variance components be modeled on 
#'                   the log-scale (defaults to \code{TRUE})? This is
#'                   applicable to the scale models with no moderators. 
#'                   See \strong{Details}.
#' 
#' @param data data frame containing the variables in the model.
#' 
#' @details
#' 
#' "\strong{Scale}"
#' 
#' The scale corresponds to the variance of a normal distribution. However,
#' in **blsmeta** it is modeled on the standard deviation scale. As a 
#' result, the reported estimates are also on the standard deviation 
#' (log) scale. To make sense of the estimates, it is helpful to 
#' use the predict function.
#' 
#' \strong{log_linear}
#' 
#' In the two and three-level models, by default a log-linear model is fitted
#' to the random-effects variances ("scale"). When no moderators are included in 
#' \code{mods_scale2} and \code{mods_scale3}, this is an intercept only model 
#' (the "scale" is constant across the \code{k} studies). 
#' 
#' To use a different prior distribution (as opposed to the default log-normal), 
#' this can be changed by setting \code{log_linear = FALSE}. In this case,
#' a half Student-t prior is employed which is then similar to the R 
#' package \strong{brms}. Note that a log-linear model is required when 
#' moderators are included in \code{mods_scale2} and \code{mods_scale3}.
#' 
#' \strong{mods_scale3}
#' 
#' Moderators for \code{mods_scale3} are inherently level 3 predictors. 
#' This means that the study-level characteristic cannot \strong{not} vary within study. 
#' For example, with, say, 100 effect sizes from 25 studies, this variance component is 
#' predicted with only the 25 studies. To do so, the first row of every study 
#' is used by default.
#' 
#' @note 
#' Three-level meta-analyses are described in \insertCite{van2013three;textual}{blsmeta}, 
#' \insertCite{cheung2014modeling;textual}{blsmeta}, and 
#' \insertCite{assink2016fitting;textual}{blsmeta}.They allow for modeling dependent 
#' effect sizes (several from the same study).
#' 
#' 
#' @references
#' \insertAllCited{}
#'   
#' @return An object of class \code{blsmeta}. This is used internally,
#' and it is not all that useful otherwise.
#' 
#' @export
#' @examples
#' # data
#' library(psymetadata)
#' 
#'   
#' fit <- blsmeta(yi = yi, vi = vi, 
#'                es_id = es_id, 
#'                data = gnambs2020, 
#'                chains = 2)
#'                
#' @importFrom rjags jags.model coda.samples    

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
    
    dat_list <- data_helper(data = data, 
                            arg = args)
    
    prior  <- prior_helper_loc(prior = prior, 
                               mm = X_location, 
                               mu = mean(dat_list$y))
    
   model_code <- paste0("model{\n", fe_rjags,
                         "\n", "#location priors\n",
                          prior, 
                         "\n}")
    
    design_mats <- list(X_location = X_location)
    
    dat_list_jags <- c(dat_list , design_mats, K = k)
    
    params <- c("beta")

    message("blsmeta: Adaptation (Fixed-Effects)")
    
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
                            dat_list = dat_list,
                            model_code = model_code
                            ) 
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
      
      dat_list <- data_helper(data = data, arg = args)
      
      design_mats <- list(X_location = X_location, 
                          X_scale_2 = X_scale2)
      
      dat_list_jags <- c(dat_list , design_mats, K = k)
      
      prior_location  <- prior_helper_loc(prior = prior, 
                                          mm = X_location, 
                                          mu = mean(dat_list$y))
      
      prior_scale2 <- prior_helper_scale_2(prior, X_scale2)
      
      priors <- paste0("#location priors\n", 
                       prior_location, "\n#scale level two priors\n",  
                       prior_scale2 )
            
      model_code <- paste0("model{\n", two_level_rjags,
                           "\n", 
                           priors, 
                           "\n}")
      
      params <- c("beta", "gamma", "re_2", "tau_2")
      
      message("blsmeta: Adaptation (Two-Level)")
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
                              dat_list = dat_list, 
                              model_code = model_code, 
                              k = k) 
      # three level
    } else {
      
     
      
      lvl3 <- eval(args[[match("study_id", names(args))]], 
                              envir = data)
      if(!is.numeric(lvl3)){
        stop("study_id must be numeric.")
      }
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
      
      dat_list <- data_helper(data = data, arg = args)
      
      dat_check <- data
      
      dat_check$study_id_new <-  dat_list$study_id
      
      ids <- unique(dat_list$study_id)
      
      if(!all(check_level_3(mods_scale3, dat_check = dat_check) == 1)){
        stop("invalid level 3 moderator. see documentation.")
      }
      
      design_mats <- list(X_location = X_location, 
                          X_scale_2 = X_scale2, 
                          X_scale_3 = X_scale3)
      
      dat_list_jags <- c(dat_list , 
                         design_mats, 
                         K = k, 
                         J = J)
      
      prior_location  <- prior_helper_loc(prior = prior, 
                                          mm = X_location, 
                                          mu = mean(dat_list$y))
      
      prior_scale2 <- prior_helper_scale_2(prior, X_scale2)
      
      prior_scale3 <- prior_helper_scale_3(prior, X_scale3)
      
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
      
      message("blsmeta: Adaptation (Three-Level)")
      
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
                              model_code = model_code, 
                              k = k, J = J) 
      
      
  }
    
  }
 
  class(returned_object) <- c("blsmeta", "default")
  return(returned_object)
}