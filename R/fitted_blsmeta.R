#' @title Expected Values of the Predictive Distribution
#' 
#' @description Compute the expected (analogous to "fitted") values 
#' for a \code{blsmeta} object.
#' 
#' @param object An object of class \code{blsmeta}.
#' 
#' @param newdata An optional data.frame for which to compute
#'                fit fitted values. Defaults to \code{NULL}, which then 
#'                uses the original data used in \code{\link[blsmeta]{blsmeta}}.
#' 
#' @param es_id logical. Should the level two "random" effect be included 
#'              (defaults to \code{TRUE})
#' 
#' @param study_id logical. Should the level three "random" effects be included 
#'                 (defaults to \code{TRUE})?
#' 
#' @param summary logical. Should the posterior samples be summarized 
#'                 (defaults to \code{TRUE})?
#' 
#' 
#' @param cred numeric. credible interval (defaults to \code{0.95}).
#' 
#' @param digits numeric. The desired number of digits for the summarized 
#'               estimates (defaults to \code{3}).
#' 
#' @param ... Currently ignored.
#'
#' @return A data frame of fitted values (when \code{summary = TRUE}),
#' 
#' 
#' @export
#'
#' @examples
#' library(psymetadata)
#' 
#' dat <- gnambs2020
#' 
#' fit <- blsmeta(yi = yi,
#'                vi = vi,
#'                es_id = es_id,
#'                data = dat)
#'                
#' fitted(fit)
fitted.blsmeta <- function(object, 
                           newdata = NULL, 
                           es_id = TRUE,
                           study_id = TRUE,
                           summary = TRUE, 
                           cred = 0.95, 
                           digits = 3, 
                           ...){
  
  if(object$model == "fe"){
    
    if (is.null(newdata)) {
      fitted_values <-
        t(object$X_location_old %*% t(.extract_beta(object)))
      
    } else {
      fitted_values <-
        t(model.matrix(object$mods_f, newdata) %*% t(.extract_beta(object)))
      
    }
    
    # end fe  
  } else if (object$model == "two_level"){
    
    re_2 <- .extract_re_2(object)
    
    betas <- .extract_beta(object)
    
    if(ncol(betas) == 1){
    
      if(es_id){
        
        fitted_values <- coef(object, summary = FALSE)
        
      } else {
        
        fitted_values <- replicate(object$k, betas, simplify = TRUE)  
        
      } # end es_id
      
    }  else {
      
      if(is.null(newdata)){
        mm <- object$X_location_old
      } else {
        
        if(!any(colnames(newdata) == "es_id" )){
          stop( "es_id not found in new data. option include: \n", 
                paste0( object$dat_list$es_id, collapse = ", "), 
                call. = FALSE)
          }
        
        mm <- model.matrix(object$mods_f, newdata)
        re_2 <- as.matrix(re_2[,newdata$es_id]) 
      }
      
      if(es_id){
        
        fitted_values <- sapply(1:nrow(mm), function(x){
            mm[x,] %*%  t(cbind(c(re_2[,x] + betas[,1]), betas[,-1]))
        })
        
      } else {
        
        fitted_values <- t(mm %*% t(betas))
        
      }
      
      
      
  }
  # three level
  } else {
  
    re_2 <- .extract_re_2(object)
    re_3 <- .extract_re_3(object)
    
    
    
    betas <- .extract_beta(object)
    
    k_per_study <- tapply(1:object$k , 
                          object$dat_list$study_id, 
                          length)
    
    re_3 <- do.call(cbind,
            sapply(1:object$J, function(x){
              replicate(k_per_study[x], re_3[,x])
            }))
    
    if(ncol(betas) == 1){
      
      if(isTRUE(es_id) & isTRUE(study_id)){
       
        fitted_values <- sapply(1:object$k, function(x){
         
          fitted_values <- re_2[,x] + re_3[,x]  + betas
          
        })
        
      } else if(isTRUE(es_id) & isFALSE(study_id)){
        
        fitted_values <- sapply(1:object$k, function(x){
          
          fitted_values <- re_2[,x] + betas
          
        })
        
      } else if(isFALSE(es_id) & isTRUE(study_id)) {
        
        fitted_values <- sapply(1:object$k, function(x){
          
          fitted_values <- re_3[,x] + betas
          
        })
        
      } else {
        
        fitted_values <- replicate(object$k, betas, simplify = TRUE)  
        
      }
      
    } else {
    
      if(is.null(newdata)){
        mm <- object$X_location_old
      } else {
        
        if(!any(colnames(newdata) == "es_id" )){
          stop( "es_id not found in new data. option include: \n", 
                paste0( object$dat_list$es_id, collapse = ", "), 
                call. = FALSE)
        }
        
        if(!any(colnames(newdata) == "study_id" )){
          stop( "study not found in new data. option include: \n", 
                paste0( object$dat_list$study_id, collapse = ", "), 
                call. = FALSE)
        }
        
        mm <- model.matrix(object$mods_f, newdata)
        re_2 <- as.matrix(re_2[,newdata$es_id]) 
        re_3 <- as.matrix(re_3[,newdata$study_id]) 
        
      }
      
      if(isTRUE(es_id) & isTRUE(study_id)){
      fitted_values <- sapply(1:nrow(mm), function(x){
        mm[x,] %*%  t(cbind(c(re_2[,x] + re_3[,x] + betas[,1]), betas[,-1]))
      })
      
    } else if (isTRUE(es_id) & isFALSE(study_id)){
      fitted_values <- sapply(1:nrow(mm), function(x){
        mm[x,] %*%  t(cbind(c(re_2[,x] + betas[,1]), betas[,-1]))
      })
        
    } else if (isFALSE(es_id) & isTRUE(study_id)) {
        
      fitted_values <- sapply(1:nrow(mm), function(x){
        mm[x,] %*%  t(cbind(c(re_3[,x] + betas[,1]), betas[,-1]))
      })
      
    } else {
      fitted_values <- replicate(object$k, betas, simplify = TRUE) 
      
      }
      
  }
    
}
  
  if(summary){
     fitted_values <- round(
        .summary_helper(fitted_values, cred = cred),
        digits = digits)
    }
  
  return(fitted_values)
}
