#' @title Make JAGS Model Code
#' 
#' @inheritParams blsmeta
#' 
#' @return A character string with the model code.
#' 
#' @export
#'
#' @examples
#' library(psymetadata)
#' 
#' prior <- 
#'   c(assign_prior(param = "(Intercept)", 
#'                  prior = "dnorm(0, 1)", dpar = "location"),
#'                  assign_prior(param = "(Intercept)", 
#'                  prior = "dnorm(0, 10)", 
#'                  dpar = "scale", level = "two")
#'                  )
#' 
#' make_jagscode(yi = yi, 
#'            vi = vi, 
#'            prior = prior,
#'            es_id = es_id,
#'            study_id = study_id,
#'            data = gnambs2020)
make_jagscode <- function(yi, vi, 
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
                       data){
  
  #note: somewhat redundant code
  #(each model is self contained)
  args <- match.call()
  
  k <- nrow(data)
  
  # fixed effects model
  if (is.null(args$es_id)) {
    if (is.null(args$mods)) {
      X_location <- model.matrix(~ 1, data)
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
    
    priors  <- prior_helper_loc(prior = prior,
                                mm = X_location,
                                mu = mean(dat_list$y))
    
    # two level
  } else {
    if (is.null(args$study_id)) {
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
      
      if (is.null(args$mods_scale2)) {
        X_scale2 <- model.matrix( ~ 1, data)
        X_scale2_old <- X_scale2
        X_scale2_means <- 1
      } else {
        X_scale2 <- model.matrix(mods_scale2, data)
        if (all(X_scale2[, 1] == 1)) {
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
                       prior_location,
                       "\n#scale level two priors\n",
                       prior_scale2)
      
    } else {
      lvl3 <- eval(args[[match("study_id", names(args))]],
                   envir = data)
      if (!is.numeric(lvl3)) {
        stop("study_id must be numeric.")
      }
      dat3 <- cbind.data.frame(lvl3 = lvl3, data)
      dat3 <- dat3[!duplicated(dat3$lvl3), ]
      J <- nrow(dat3)
      
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
      
      if (is.null(args$mods_scale2)) {
        X_scale2 <- model.matrix( ~ 1, data)
        X_scale2_old <- X_scale2
        X_scale2_means <- 1
      } else {
        X_scale2 <- model.matrix(mods_scale2, data)
        if (all(X_scale2[, 1] == 1)) {
          center_X <- center_helper(X_scale2)
          X_scale2 <- center_X$x
          X_scale2_old <- center_X$xold
          X_scale2_means <- center_X$x_mean
        } else {
          X_scale2_old <- X_scale2
          X_scale2_means <- NULL
        }
      }
      
      if (is.null(args$mods_scale3)) {
        X_scale3 <- model.matrix( ~ 1, dat3)
        X_scale3_old <- X_scale3
        X_scale3_means <- 1
      } else {
        X_scale3 <- model.matrix(mods_scale3, dat3)
        if (all(X_scale3[, 1] == 1)) {
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
      
      if (!all(check_level_3(mods_scale3, dat_check = dat_check) == 1)) {
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
      
      priors <- paste0(
        "#location priors\n",
        prior_location,
        "\n\n#scale level two priors\n",
        prior_scale2,
        "\n\n#scale level three priors\n",
        prior_scale3
      )
      
      model_code <- paste0("model{\n", three_level_rjags,
                           "\n",
                           priors,
                           "\n}")
    }
    
  }
  returned_object <- list(model_code = model_code)
  class(returned_object) <- c("blsmeta", 
                              "model_code")
  return(returned_object)
  
}