#' @title Variance Components
#' 
#' @description Typically, the variance components are assumed constant
#' across the \code{k} studies in meta-analysis. When scale modeling,
#' the variance components are function of
#' moderators, which can be computed with this function 
#' \insertCite{@see @williams2021putting}{blsmeta}.
#' 
#' @param object An object of class \code{blsmeta}.
#' 
#' @param type  character. Should the variance or standard deviation? The options 
#'             are \code{type = "var"} and \code{type = "sd"} (the default).
#' 
#' @param newdata_scale2 An optional data.frame for which to compute
#'                       predictions for the level 2 variance component. 
#'                       Defaults to \code{NULL}, which then uses the 
#'                       original data used in \code{\link[blsmeta]{blsmeta}}
#' 
#' @param newdata_scale3 An optional data.frame for which to compute
#'                       predictions for the level 3 variance component. 
#'                       Defaults to \code{NULL}, which then uses the 
#'                       original data used in \code{\link[blsmeta]{blsmeta}}
#' 
#' @param cred numeric. credible interval (defaults to \code{0.95}).
#' 
#' @param summary logical. Should the posterior samples be summarized 
#'                 (defaults to \code{TRUE})?
#' 
#' @param digits numeric. The desired number of digits for the summarized 
#'               estimates (defaults to \code{3}).
#'
#' @references
#' \insertAllCited{}      
#''
#' @return A data frame of predicted values.
#' 
#' @export
#' 
#' @examples
#' 
#' library(psymetadata)
#' 
#' fit <- blsmeta(yi = yi, vi = vi, 
#'                es_id = es_id,
#'                mods_scale2 = ~n,
#'                data = gnambs2020)
#' 
#' 
#' tau2(object = fit, 
#'      newdata_scale2 = data.frame(n = seq(20, 100, 10)))
tau2 <- function(object, 
                 type = "sd",
                 newdata_scale2 = NULL, 
                 newdata_scale3 = NULL, 
                 cred = 0.95,
                 summary = TRUE,
                 digits = 3) {
  
  
  if (!is(object, "blsmeta")) {
    stop("object must be of class 'blsmeta'")
  }
  
  if (object$model == "fe") {
    stop("fixed-effects models not supported")
  }
  
  if(object$model == "two_level"){
    
    if(is.null(newdata_scale2)){
      scale2 <- exp(.extract_scale2(object))^2
    } else {
      
      if (!is.data.frame(newdata_scale2)) {
        stop("newdata_scale2 must be a data.frame.")
      }
      
      mods_scale2_newdata <- model.matrix(object$mods_scale2_f,
                                          newdata_scale2)
      
      gammas <- .extract_gamma(object)
      scale2 <- exp(t(mods_scale2_newdata %*% t(gammas)))^2
      
      
    }
    
    if(type == "sd"){
      tau2 <- sqrt(scale2)
    } else {
      tau2 <- round(scale2, digits = digits)
    }
      
     if(summary){
       tau2 <- round(
         .summary_helper(tau2, cred = cred), 
         digits = digits
         )
     } 
    
    }  else {
    
    if (is.null(newdata_scale2)) {
      scale2 <- exp(.extract_scale2(object))^2
    }  else {
      if (!is.data.frame(newdata_scale2)) {
        stop("newdata_scale2 must be a data.frame.")
      }
      
      mods_scale2_newdata <- model.matrix(object$mods_scale2_f,
                                          newdata_scale2)
      gammas <- .extract_gamma(object)
      scale2 <- exp(t(mods_scale2_newdata %*% t(gammas)))^2
      
    }
    
    
    if(is.null(newdata_scale3)) {
      
      k_per_study <- tapply(1:object$k , 
                            object$dat_list$study_id, length)
      
      scale3 <- .extract_scale3(object)
      
      level_3_sd <- 
        exp(
          do.call(cbind,
                  sapply(1:object$J, function(x){
                    replicate(k_per_study[x], scale3[,x])
                  })))
      
      scale3 <- level_3_sd^2
      
    } else {
      if (!is.data.frame(newdata_scale3)) {
        stop("newdata_scale3 must be a data.frame.")
      }
      mods_scale3_newdata <- model.matrix(object$mods_scale3_f,
                                          newdata_scale3)
      etas <- .extract_eta(object)
      scale3 <- exp(t(mods_scale3_newdata %*% t(etas)))^2
    }
    
    
    if(type == "sd"){
      tau2_2 <- sqrt(scale2)
      tau2_3 <- sqrt(scale3)
    } else {
      tau2_2 <- scale2
      tau2_3 <- scale3
    }
      
    if(summary){  
    tau2 <- list()
    
    tau2[[1]] <- .summary_helper(tau2_2, cred = cred)
    tau2[[2]] <- .summary_helper(tau2_3, cred = cred)
    }
    
      tau2 <- lapply(1:2, function(x) {
        round(tau2[[x]], digits = digits) }
      )
      
      names(tau2) <- c("level_two", "level_three")
    
  }
  
  return(tau2)
  
}