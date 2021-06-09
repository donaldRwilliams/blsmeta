#' @title Credible Intervals for Heterogeneity Statistics
#' 
#' @description Compute credible intervals for between-study variance, 
#'              between-study standard deviation, I2, and H2. This
#'              function mimics that of **metafor**, but for credible
#'              and not confidence intervals, as well as for two and 
#'              three-level models.
#'              
#' 
#' @param object An object of class \code{blsmeta}.
#' 
#' @param cred numeric. credible interval (defaults to \code{0.95}).
#' 
#' @param digits numeric. The desired number of digits for the summarized 
#'               estimates (defaults to \code{3}).
#' 
#' @param ... Currently ignored.
#'
#' @return An object of class \code{confint}, including a data.frame
#'         with the estimates.
#' 
#' @export
#' 
#' @note There cannot be a scale model. To get I2, etc., when there
#' is a scale model, use \link[blsmeta]{I2}.
#' 
#' @examples
#' library(psymetadata)
#' 
#' fit_re <- blsmeta(yi = yi, vi = vi, 
#'                   es_id = es_id,
#'                   data = gnambs2020)
#' 
#' credint(fit_re)
credint.blsmeta <- function(object,
                           cred = 0.95, 
                           digits = 3, 
                            ...){
  
  if (!is(object, "blsmeta")) {
    stop("object must be of class 'blsmeta'")
  }
  
  if (object$model == "fe") {
    stop("fixed-effects models not supported")
  }
  
  if(object$model == "two_level"){
  
  if(object$mods_scale2_f != ~1){
    stop("scale model not permitted. see I2, H2, and tau2 functions.")
  }
  
  s2 <- s2_helper(object$dat_list$v)
  gammas <- .extract_gamma(object)
  tau <- t(exp(matrix(1)  %*% t(gammas)))
  tau2 <- tau^2
  I2 <- (tau2 / (tau2 + s2))
  H2 <- 1 / (1 - I2)
  I2 <- I2 * 100
  returned_object <- list(estimates =
                            round(
                              rbind.data.frame(
                                "tau^2" = .summary_helper(tau2, cred),
                                tau = .summary_helper(tau, cred),
                                "I^2" = .summary_helper(I2, cred),
                                "H^2" = .summary_helper(H2, cred)
                              ),
                              digits = digits
                            ))
  
  } else {
    
    if(object$mods_scale2_f != ~1){
      stop("scale model not permitted. see I2, H2, and tau2 functions.")
    }
    
    if(object$mods_scale3_f != ~1){
      stop("scale model not permitted. see I2, H2, and tau2 functions.")
    }
    
    
    s2 <- s2_helper(object$dat_list$v)
    gammas <- .extract_gamma(object)
    etas <- .extract_eta(object)  
    
    tau_2 <- t(exp(matrix(1)  %*% t(gammas)))
    tau2_2 <- tau_2^2
    
    tau_3 <- t(exp(matrix(1)  %*% t(etas)))
    tau2_3 <- tau_3^2
    
    I2_2 <- (tau2_2 / (tau2_2 + tau2_3 + s2)) * 100
    I2_3 <- (tau2_3 / (tau2_2 + tau2_3 + s2)) * 100
    H2 <- (tau2_2 + tau2_3 + s2) / s2
    
    
    level_2 <-  rbind.data.frame(
      "tau^2" = .summary_helper(tau2_2, cred),
      tau = .summary_helper(tau_2, cred),
      "I^2" = .summary_helper(I2_2, cred)
    )
    
    level_3 <-  rbind.data.frame(
      "tau^2" = .summary_helper(tau2_3, cred),
      tau = .summary_helper(tau_3, cred),
      "I^2" = .summary_helper(I2_3, cred)
    )
    
    h2 <- rbind.data.frame("H^2" = .summary_helper(H2, cred))
    returned_object <- list(level_2 = round(level_2, digits = digits), 
                            level_3 = round(level_3, digits = digits), 
                            h2 = round(h2, digits = digits))
    
    
  }
  
  class(returned_object) <- c("blsmeta", "confint")
  return(returned_object)
  
}


#' @title S3 \code{credint} method
#'
#' @param object An object of class \code{blsmeta}
#' @param ... Currently ignored
#' @export
credint <- function(object, ...){
  UseMethod("credint", object)
}

