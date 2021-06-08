#' @title Compute the Total Variance in \code{yi}
#'
#' 
#' @description Typically, the variance components are assumed constant
#' across the \code{k} studies in meta-analysis, implying that
#' the total variance for each effect size is the sum of the
#' sampling and between-study variance. When scale modeling,
#' the variance in \code{yi} is now a function of those
#' moderators, which can be computed with this function 
#' \insertCite{@see @williams2021putting}{blsmeta}.
#'
#' @param object object of class \code{blsmeta}.
#' 
#' @param type character. Should the variance or standard deviation? The options 
#'             are \code{type = "var"} and \code{type = "sd"} (the default).
#' 
#' @param cred numeric. credible interval (defaults to \code{0.95}).
#' 
#' @param summary logical. Should the posterior samples be summarized 
#'                 (defaults to \code{TRUE})?
#'
#' @return Either a summarized data frame, including the 
#'         posterior mean, sd, and credible intervals, or a matrix
#'         of dimensions \code{iter} * \code{chains} by \code{k}.
#' 
#' @note The sampling variances are assumed to be known.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @export
#'
#' @examples
#' library(psymetadata)
#' 
#' # no scale model
#' fit <- blsmeta(yi, vi, 
#'                es_id = es_id, 
#'                mods_scale2 = ~ 1,
#'                data = gnambs2020)
#' 
#' no_scale_mod <- variance_yi(fit)
#' 
#' # scale model
#' fit <- blsmeta(yi, vi, 
#'                es_id = es_id, 
#'                mods_scale2 = ~ n,
#'                data = gnambs2020)
#' 
#' scale_mod <- variance_yi(fit)
variance_yi <- function(object, 
                        type = "sd", 
                        cred =  0.95, 
                        summary = TRUE){
  
  if (!is(object, "blsmeta")) {
    stop("object must be of class 'blsmeta'")
  }
  
  if (object$model == "fe") {
    stop("fixed-effects models not supported")
  }
  
  if(object$model == "two_level"){
    
    scale2 <- .extract_scale2(object)
    
    total_var <- 
      exp(scale2)^2 + 
      matrix(object$dat_list$v, 
             object$iter * object$chains, 
             object$k, 
             byrow = TRUE)
    
    if(type == "sd"){
      total_var <- sqrt(total_var)
    }
    
    if(summary){
      total_var <- .summary_helper(total_var, cred = cred)
      }
  } else if (object$model == "three_level") {
    
    scale2 <- .extract_scale2(object)
    
    scale3 <- .extract_scale3(object)
    
   level_2_var <- exp(scale2)^2 
    
    level_3_sd <- 
      exp(
        do.call(cbind,
                sapply(1:object$J, function(x){
                  replicate(k_per_study[x], scale3[,x])
                })))
    
    level_3_var <- level_3_sd^2
   
    
    vi <- matrix(object$dat_list$v, 
           object$iter * object$chains, 
           object$k, 
           byrow = TRUE)
    
    total_var <- level_3_var + level_2_var + vi
    
    if(type == "sd"){
      total_var <- sqrt(total_var)
    }
    
    if(summary){
      total_var <- .summary_helper(total_var, cred = cred)
    }
    
  }
  return(total_var)
}
