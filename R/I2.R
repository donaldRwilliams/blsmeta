#' @title I2 Heterogeneity Statistic
#' 
#' @description Compute I2 (total heterogeneity divided by total variability). 
#' Note that the I2 formulation for three-level models is described in
#' \insertCite{cheung2014modeling;textual}{blsmeta}.
#' 
#' 
#' @param object An object of class \code{blsmeta}.
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
#' @param s2 numeric. A user-defined "typical" sampling variance.
#'           Defaults to the estimator in Equation 9 of 
#'           \insertCite{Higgins2002a;textual}{blsmeta}.
#' 
#' @param cred numeric. credible interval (defaults to \code{0.95}).
#' 
#' @param summary logical. Should the posterior samples be summarized 
#'                 (defaults to \code{TRUE})?
#' 
#' @param percent logical. Should the results be percentages, as in **metafor**
#'                 (defaults to \code{TRUE})?
#' 
#' @param digits numeric. The desired number of digits for the summarized 
#'               estimates (defaults to \code{3}).
#'                
#' @note The sampling variances are assumed to be known.
#' 
#' @references
#' \insertAllCited{}      
#''
#' @return A data frame of predicted values.
#' 
#' @export
#'
#' @examples
#' # data
#' library(psymetadata)
#'
#' # no scale model
#' fit <- blsmeta(yi, vi,
#'                es_id = es_id,
#'                data = gnambs2020)
#' 
#' # compute I2 for all data
#' i2 <- I2(fit)
#' 
#' # scale model
#' fit <- blsmeta(yi, vi,
#'                es_id = es_id,
#'                mods_scale2 = ~ n,
#'                data = gnambs2020)
#' 
#' new_data <- data.frame(n = c(100, 150))
#' 
#' # compute I2 for new data
#' i2 <- I2(fit, newdata_scale2 = new_data)
I2 <- function(object, 
               newdata_scale2 = NULL, 
               newdata_scale3 = NULL, 
               s2 = NULL,
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
  
  if (is.null(s2)) {
    s2 <- s2_helper(vi = object$dat_list$v, 
                    method = "ht")
  }
  
  if (object$model == "two_level") {
    if (is.null(newdata_scale2)) {
      scale2 <- exp(.extract_scale2(object)) ^ 2
    } else {
      if (!is.data.frame(newdata_scale2)) {
        stop("newdata_scale2 must be a data.frame.")
      }
      
      mods_scale2_newdata <- model.matrix(object$mods_scale2_f,
                                          newdata_scale2)
      
      gammas <- .extract_gamma(object)
      scale2 <- exp(t(mods_scale2_newdata %*% t(gammas)))^2
      
    }
    
    
    
    i2 <-
      as.matrix(apply(scale2, 1, function(x) {
        i2 <- x / (x + s2)
        if (percent) {
          i2 <- i2 * 100
        }
        return(i2)
      }))
    
    if (summary) {
      if (ncol(i2) == 1) {
        i2 <- .summary_helper(i2, cred = cred)
      }
      i2 <- .summary_helper(t(i2), cred = cred)
      }
  } else if(object$model == "three_level"){
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
    
    if(missing(newdata_scale3)) {
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
    
   i2_2 <- as.matrix(
   sapply(1:ncol(scale2), function(x){
     i2 <- scale2[,x] / (tot_var[,x] + s2)
     
     if (percent) {
       i2 <- i2 * 100
     }
     return(i2)
     
   }))
   
   i2_3 <- as.matrix(
     sapply(1:ncol(scale3), function(x){
       i2 <- scale3[,x] / (tot_var[,x] + s2)
       if (percent) {
         i2 <- i2 * 100
       }
       return(i2)
       
     }))
   
   if(summary){
    i2 <- list()
    if(ncol(i2_2) == 1){
      i2_2 <- .summary_helper(i2_2, cred = cred)
    } else {
      i2_2 <- .summary_helper(i2_2, cred = cred)
    }
    if(ncol(i2_3) == 1){
      i2_3 <- .summary_helper(i2_3, cred = cred)
    } else {
      i2_3 <- .summary_helper(i2_3, cred = cred)
    }
    
    i2[[1]] <- i2_2
    i2[[2]] <- i2_3
    
    i2 <- lapply(1:2, function(x) {
      round(i2[[x]], digits = digits) }
    )
    
    names(i2) <- c("level_two", "level_three")
    
    }
  } else {
      stop("model not supported.")
    }
  
 
 
  return(i2)
}


