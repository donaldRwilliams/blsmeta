#' Title
#'
#' @param object 
#' @param mod 
#' @param mod_tau_2 
#' @param data 
#' @param re_formula 
#' @param summary 
#' @param cred 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
posterior_predict <- function(object,
                              mod = NULL,
                              mod_tau_2 = NULL,
                              data = NULL,
                              re_formula = NULL,
                              summary = TRUE,
                              cred = 0.95,
                              ...) {
  # check class
  if (!is(object, "blsmeta")) {
    stop("invalid class. must be 'blsmeta'")
  }
  
  # if(is.null(data)){
  #   data <- object$data
  # }
  
  samps <- extract_samples(object)
  
  re_2 <- extract_re_2(samps)
  
  betas <- extract_beta(samps, object$mean_X)
  
  gammas <- extract_gamma(samps, object$mean_X2)
  
  
  # old data
  if (is.null(data)) {
    if (is.null(re_formula)) {
      if (isFALSE(object$save_ranef)) {
        stop("save_ranef = FALSE. set to TRUE")
      }
      
      
      
      p_beta <- object$model$data()$p_beta
      
      k <- ncol(re_2)
      
      vi <- object$model$data()$v
      
      nsamps <- nrow(re_2)
      
      yrep <-
        sapply(1:k, function(x) {
          rnorm(
            n = nsamps,
            mean = object$xold[x, ] %*% t(cbind(betas[, 1] + re_2[, x], betas[, 2:p_beta])),
            sd =  sqrt(vi[x])
          )
        })
    } else if (is.na(re_formula)) {
      yrep <- t(object$xold %*% t(betas))
      
    } else {
      stop("invalid 'type'. must be 'metafor' or 'brms'")
    }
    # new data
  } else {
    if (is.null(mod)) {
      X <- model.matrix(as.formula(object$arg$mod), data = data)
      
    } else {
      X <- model.matrix(mod, data)
      
      if (!all(colnames(X) == colnames(object$xold))) {
        stop("different mod_tau_2. must be the same in both models")
      }
      
    }
    
    
    
    if (is.null(mod_tau_2)) {
      X2 <- model.matrix(as.formula(object$arg$mod_tau_2), data = data)
      
    } else {
      X2 <- model.matrix(mod_tau_2, data)
      
      if (!all(colnames(X2) == colnames(object$x2old))) {
        stop("different mod_tau_2. must be the same in both models")
      }
    }
    
    
    
    
    level_2_var <- t(exp(X2 %*% t(gammas))) ^ 2
    
    yhat <- t(X %*% t(betas))
    
    nrow <- nrow(data)
    nsamps <- nrow(yhat)
    
    if (is.null(data$v)) {
      message("using 'typical' sampling variance")
      
      s2 <- rep(s2_helper(vi), nrow)
      
    } else {
      s2 <- data$v
    }
    
    
    yrep <- sapply(1:nrow, function(x) {
      if (is.null(re_formula)) {
        yrep <- rnorm(nsamps, yhat[, x],  sqrt(level_2_var[, x] + s2[x]))
      } else {
        yrep <- rnorm(nsamps, yhat[, x],  sqrt(s2[x]))
      }
      return(yrep)
    })
    
    
  }
  
  if (summary) {
    creds <- cred_helper(cred)
    lb <- creds[1]
    ub <- creds[2]
    
    yrep <- data.frame(
      Post.mean = colMeans(yrep),
      Post.sd = apply(yrep, 2, sd),
      Cred.lb = apply(yrep, 2,  quantile, lb),
      Cred.ub = apply(yrep, 2,  quantile, ub)
    )
  }
  return(yrep)
  
}


