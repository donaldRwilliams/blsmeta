#' @title Credible Intervals for Heterogeneity Statistics
#' 
#' @description Compute credible intervals for between-study variance, 
#'              between-study standard deviation, I2, and H2. This
#'              function mimics that of **metafor**, but for credible
#'              and not confidence intervals.
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
#' @examples
#' library(psymetadata)
#' 
#' fit_re <- blsmeta(yi = yi, vi = vi, 
#'                   es_id = es_id,
#'                   data = gnambs2020)
#' 
#' confint(fit_re)
confint.blsmeta <- function(object, 
                            cred = 0.95, 
                            digits = 3, ...){
  
  if (!is(object, "blsmeta")) {
    stop("object must be of class 'blsmeta'")
  }
  
  if (object$model == "fe") {
    stop("fixed-effects models not supported")
  }
  if (object$model == "three_level") {
    stop("three-level models not supported")
  }
  
  if(fit_re$mods_scale2_f != ~1){
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
  
  class(returned_object) <- c("blsmeta", "confint")
  return(returned_object)
  
}

