#' @title  Intraclass Correlation Coefficients
#' 
#' @description Compute ICCs for three-level meta-analysis 
#' Note that the ICC formulation for three-level models is described in
#' \insertCite{cheung2014modeling;textual}{blsmeta}. It is defined as
#' the heterogeneity at each level divided by the total heterogeneity,
#' which will approach I2 when the 
#' sampling variances approach zero (large n).
#'
#' @param object An object of class \code{blsmeta}.
#' 
#' @param newdata_scale2 An optional data.frame for which to compute
#'                       predictions for the level 2 variance component. 
#'                       Defaults to \code{NULL}, which then uses the 
#'                       original data used in \code{\link[blsmeta]{blsmeta}}.
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
#' @param percent  logical. Should the results be percentages, as in **metafor**
#'                 (defaults to \code{TRUE})?
#' 
#' @param digits numeric. The desired number of digits for the summarized 
#'               estimates (defaults to \code{3}).
#'
#' @return A list of two data frames of predicted values.
#' 
#' @details In essence, with a scale model, this results in the
#' ICCs being a function of moderators. Further, rather than one 
#' ICC, there is an ICC for each of the \code{k} studies.  
#' For more information about varying ICCs, 
#' we refer interested users to \insertCite{williams2019putting;textual}{blsmeta} 
#' and \insertCite{williams2020fine}{blsmeta}.
#' 
#' @references
#' \insertAllCited{}  
#' 
#' @export
#'
#' @examples
#' library(psymetadata)
#' 
#' fit <- blsmeta(yi = yi, vi = vi, 
#'                es_id = es_id,
#'                study_id = study_id,
#'                data = gnambs2020)
#' 
#' 
#' iccs <- ICC(object = fit, 
#'             summary = TRUE)
ICC <- function(object, 
                newdata_scale2 = NULL, 
                newdata_scale3 = NULL, 
                cred = 0.95,
                summary = TRUE,
                percent = TRUE, 
                digits = 3) {
  if (!is(object, "blsmeta")) {
    stop("object must be of class 'blsmeta'")
  }
  
  if (object$model == "fe") {
    stop("fixed-effects models not supported")
  }
  
  if (object$model == "two_level") {
    stop("two-level models not supported")
  }
  
  if (is.null(newdata_scale2)) {
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
  
  if(ncol(scale3) != ncol(scale2)){
    stop("newdata must have the same number of rows.")
  }
  
  tot_var <- scale3 + scale2
  
  icc2 <- as.matrix(
    sapply(1:ncol(scale2), function(x){
      icc2 <- scale2[,x] / (tot_var[,x])
      
      if (percent) {
        icc2 <- icc2 * 100
      }
      return(icc2)
      
    }))  
  
  icc3 <- as.matrix(
    sapply(1:ncol(scale3), function(x){
      icc3 <- scale3[,x] / (tot_var[,x])
      
      if (percent) {
        icc3 <- icc3 * 100
      }
      return(icc3)
      
    }))  
  
  if(summary){
    icc <- list()
    if(ncol(icc2) == 1){
      icc2 <- .summary_helper(icc2, cred = cred)
    } else {
      icc2 <- .summary_helper(icc2, cred = cred)
    }
    if(ncol(icc3) == 1){
      icc3 <- .summary_helper(icc3, cred = cred)
    } else {
      icc3 <- .summary_helper(icc3, cred = cred)
    }
    
    icc[[1]] <- icc2
    icc[[2]] <- icc3
    
    icc <- lapply(1:2, function(x) {
      round(icc[[x]], digits = digits) }
    )
    
    } else {
    icc <- list()
    icc[[1]] <- icc2
    icc[[2]] <- icc3
  }
  
  names(icc) <- c("level_two", 
                  "level_three")
  
  returned_object <- list(icc = icc)
  
  class(returned_object) <- c("blsmeta", "icc")
  return(returned_object)
  
}
