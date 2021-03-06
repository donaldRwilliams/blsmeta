#' Print \code{blsmeta} Objects
#' 
#' @param x An object of class \code{blsmeta}
#' 
#' @param cred numeric. credible interval (defaults to \code{0.95}).
#' 
#' @param ... currently ignored.
#'
#' 
#' @export
#'
#'@importFrom stats quantile sd
print.blsmeta <- function(x, cred = 0.95, ...) {
  if(class(x)[2] == "default"){
  
  lb <- (1 - cred) / 2
  ub <- 1 - lb
  if(x$model == "fe"){
    cat("Model: Fixed-Effects\n")
    cat("Studies:", nrow(x$X_location), "\n")
    cat(paste0( "Samples: ", x$chains * x$iter, " (", x$chains," chains)", "\n"))
    cat("Formula:", paste0( x$mods_f, collapse = " "), "\n")
    cat("------\n")
    betas <- .extract_beta(x)
    rhat <- coda::gelman.diag(x$posterior_samples)
    
    beta_summary <- 
        format(round(
          data.frame(
            apply(betas, 2, mean),
            apply(betas, 2, sd),
            t(apply(betas, 2, quantile,  c(0.025, 0.975))),
            rhat$psrf[,1]
          ), digits = 2), nsmall = 2)
    
    colnames(beta_summary) <- c("Post.mean", 
                                "Post.sd", 
                                "Cred.lb", 
                                "Cred.ub", 
                                "Rhat")
    
    rownames( beta_summary) <- colnames(x$X_location)
    cat("Location:\n")
    print(beta_summary)
    cat("\n------\n")
  } else if (x$model == "two_level"){
    cat("Model: Two-Level\n")
    cat("Studies:", nrow(x$X_location), "\n")
    cat(paste0("Samples: ", x$chains *  x$iter, " (", x$chains," chains)", "\n"))
    cat("Location Formula:", paste0( x$mods_f, collapse = " "), "\n")
    cat("Scale Formula:", paste0( x$mods_scale2_f, collapse = " "), "\n")
    cat("Note: 'Scale' on standard deviation scale")
    cat("\n------\n")
    
    betas <- .extract_beta(x)
    betas_rhat <- lapply(1:x$chains , function(i){
      x$posterior_samples[[i]][,grep("beta", coda::varnames(x$posterior_samples))]
    })
    
    betas_rhat <- coda::gelman.diag(betas_rhat, multivariate = FALSE)
    
    
    gammas <- .extract_gamma(x)
    
    if(ncol(gammas)==1){
      gammas <- exp(gammas)
    }
    
    gammas_rhat <- lapply(1:x$chains , function(i){
      x$posterior_samples[[i]][,grep("gamma", coda::varnames(x$posterior_samples))]
    })
    
    gammas_rhat <- coda::gelman.diag(gammas_rhat, multivariate = FALSE)
    
    beta_summary <- 
      format(round(
        data.frame(
          apply(betas, 2, mean),
          apply(betas, 2, sd),
          t(apply(betas, 2, quantile,  c(0.025, 0.975))),
          betas_rhat$psrf[,1]
        ), digits = 2), nsmall = 2)
    
    gamma_summary <- 
      format(round(
        data.frame(
          apply(gammas, 2, mean),
          apply(gammas, 2, sd),
          t(apply(gammas, 2, quantile,  c(0.025, 0.975))),
          gammas_rhat$psrf[,1]
        ), digits = 2), nsmall = 2)
    
    rownames(beta_summary) <- colnames(x$X_location)
    
    if(ncol(gammas) == 1){
      rownames(gamma_summary) <- paste0("sd", colnames(x$X_scale2))
    } else {
      rownames(gamma_summary) <-  colnames(x$X_scale2)
    }
    colnames(beta_summary) <- c("Post.mean", 
                                "Post.sd", 
                                "Cred.lb", 
                                "Cred.ub", 
                                "Rhat")
    
    colnames(gamma_summary) <- c("Post.mean", 
                                 "Post.sd", 
                                 "Cred.lb", 
                                 "Cred.ub", 
                                 "Rhat")
    cat("Scale:\n")
    print(gamma_summary)
    cat("\n")
    cat("Location:\n")
    print(beta_summary)
    cat("\n------\n")
  } else {
    cat("Model: Three-Level\n")
    cat("Studies2:", nrow(x$X_location), "\n")
    cat("Studies3:", nrow(x$X_scale3), "\n")
    cat(paste0("Samples: ", x$chains *  x$iter, " (", x$chains," chains)", "\n"))
    cat("Location Formula:", paste0( x$mods_f, collapse = " "), "\n")
    cat("Scale2 Formula:", paste0( x$mods_scale2_f, collapse = " "), "\n")
    cat("Scale3 Formula:", paste0( x$mods_scale3_f, collapse = " "), "\n")
    cat("Note: 'Scale' on standard deviation scale")
    cat("\n------\n")
    
    betas <- .extract_beta(x)
    betas_rhat <- lapply(1:x$chains , function(i){
      x$posterior_samples[[i]][,grep("beta", coda::varnames(x$posterior_samples))]
    })
    
    betas_rhat <- coda::gelman.diag(betas_rhat, multivariate = FALSE)
    
    gammas <- .extract_gamma(x)
    if(ncol(gammas)==1 | !all(x$X_scale2[,1] ==1)){
      gammas <- exp(gammas)
    }
    
    gammas_rhat <- lapply(1:x$chains , function(i){
      x$posterior_samples[[i]][,grep("gamma", coda::varnames(x$posterior_samples))]
    })
    
    gammas_rhat <- coda::gelman.diag(gammas_rhat, multivariate = FALSE)
    
    etas <- .extract_eta(x)
    
    if(ncol(etas)==1 |  !all(x$X_scale3[,1] == 1)){
      etas <- exp(etas)
    }
    
    if(ncol(etas) == 1){
      etas_rhat <- lapply(1:x$chains , function(i){
        x$posterior_samples[[i]][,"eta"]
      })
    } else {
      
      etas_rhat <- lapply(1:x$chains , function(i){
        x$posterior_samples[[i]][,paste0("eta[",1:p, "]")]
      })
    }
    
    etas_rhat <- coda::gelman.diag(etas_rhat, multivariate = FALSE)
    
    beta_summary <- 
      format(round(
        data.frame(
          apply(betas, 2, mean),
          apply(betas, 2, sd),
          t(apply(betas, 2, quantile,  c(0.025, 0.975))),
          betas_rhat$psrf[,1]
        ), digits = 2), nsmall = 2)
    
    gamma_summary <- 
      format(round(
        data.frame(
          apply(gammas, 2, mean),
          apply(gammas, 2, sd),
          t(apply(gammas, 2, quantile,  c(0.025, 0.975))),
          gammas_rhat$psrf[,1]
        ), digits = 2), nsmall = 2)
    
    eta_summary <- 
      format(round(
        data.frame(
          apply(etas, 2, mean),
          apply(etas, 2, sd),
          t(apply(etas, 2, quantile,  c(0.025, 0.975))),
          etas_rhat$psrf[,1]
        ), digits = 2), nsmall = 2)
    
    
    rownames(beta_summary) <- colnames(x$X_location)
    
    if(ncol(gammas) == 1 | !all(x$X_scale2[,1] ==1)){
      rownames(gamma_summary) <- paste0("sd", colnames(x$X_scale2))
    } else {
      rownames(gamma_summary) <-  colnames(x$X_scale2)
    }
    
    if(!all(x$X_scale2[,1] ==1)){
      rownames(gamma_summary) <- paste0("sd(", colnames(x$X_scale2),")")
    }
    
    if(ncol(etas) == 1){
      rownames(eta_summary) <- paste0("sd", colnames(x$X_scale3))
    } else {
      rownames(eta_summary) <-  colnames(x$X_scale3)
    }
    
    if(!all(x$X_scale3[,1] ==1)){
      rownames(eta_summary) <- paste0("sd(", colnames(x$X_scale3),")")
    }
    
    colnames(beta_summary) <- c("Post.mean", 
                                "Post.sd", 
                                "Cred.lb", 
                                "Cred.ub", 
                                "Rhat")
    
    colnames(gamma_summary) <- c("Post.mean", 
                                 "Post.sd", 
                                 "Cred.lb", 
                                 "Cred.ub", 
                                 "Rhat")  
    
    colnames(eta_summary) <- c("Post.mean", 
                               "Post.sd", 
                               "Cred.lb", 
                               "Cred.ub", 
                               "Rhat")  
    
    
    cat("Scale2:\n")
    print(gamma_summary)
    cat("\n")
    cat("Scale3:\n")
    print(eta_summary)
    cat("\n")
    cat("Location:\n")
    print(beta_summary)
    cat("\n------\n")
  }
 
  cat("Date:", date(), "\n")
  
  } else if(class(x)[2] == "confint"){
    if(length(x) == 1) {
      print(x$estimates)
    } else {
      cat("Level Two:\n")
      print(x$level_2)
      cat("\nLevel Three:\n")
      print(x$level_3)
      cat("\n------\n")
      print(x$h2)
    }
    # end confint
  } else if (class(x)[2] == "icc") {
    cat("Level Two:\n")
    print(x$icc$level_two)
    cat("\nLevel Three:\n")
    print(x$icc$level_three)
  } else if(class(x)[2] == "ranef"){
    if(length(x$ranefs) != 2){
      cat("Level Two:\n")
      print(x$ranefs$level_two)
    } else if(length(x$ranefs) == 2 ) {
      cat("Level Two:\n")
      print(x$ranefs$level_two)
      cat("\nLevel Three:\n")
      print(x$ranefs$level_three)

    } 
    
  } else if(class(x)[2] == "make_prior"){
    cat(x$priors)
    
  } else if(class(x)[2] == "model_code"){
    cat(x$model_code)
  }
}
