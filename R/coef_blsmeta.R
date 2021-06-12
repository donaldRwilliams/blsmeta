#' @title Extract Group-Specific Coefficients
#' 
#' @description Extract the group-specific coefficients, defined as
#' the sum of the fixed effects and corresponding random effects.
#'
#' @param object An object of class \code{blsmeta}.
#' 
#' @param summary logical. Should the posterior samples be summarized 
#'                 (defaults to \code{TRUE})?
#' 
#' @param cred numeric. credible interval (defaults to \code{0.95}).
#' 
#' @param digits numeric. The desired number of digits for the summarized 
#'               estimates (defaults to \code{3}).
#' 
#' @param ... Currently ignored.
#'
#' @return A list of class \code{blsmeta}. When \code{summary = TRUE},
#'         each object is a data frame including the summarized posterior 
#'         samples. When \code{summary = FALSE}, each object is a matrix
#'         with the posterior samples.
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
#' coef(fit)
coef.blsmeta <- function(object, 
                         summary = TRUE, 
                         cred = 0.95, 
                         digits = 3, 
                         ...){
  
  if(object$model == "fe"){
   stop("Fixed-effects models not supported (no 'random' effects)")
    
  } 
  
  if(object$model == "two_level"){
    
    betas <- .extract_beta(object)  
    re_2 <- .extract_re_2(object)
    
    if(ncol(betas) == 1){
      
      coefs <- sapply(1:ncol(re_2), function(x) {
        
        coefs <-  re_2[,x] + betas
        return(coefs)
      })
      
      if(summary){ 
        coefs <- .summary_helper(coefs, cred = cred)
      }    
      
    }  else {
      
      # intercept in model
      if(all(object$X_location[,1] == 1)){
        
        coefs <- sapply(1:ncol(re_2), function(x) {
          
          coefs <-  re_2[,x] + betas[,1]
          return(coefs)
        })
        
        if(summary){
          
          coefs <- .summary_helper(coefs, cred = cred)
        }
      } else {
        
        coefs <- .extract_re_2(object)
        
        if(summary){
          coefs <- .summary_helper(coefs, cred = cred)
        }
      }
    }
    
    coefs <- round(coefs, digits = digits)
    
  } else {
    
    betas <- .extract_beta(object)  
    re_2 <- .extract_re_2(object)
    re_3 <- .extract_re_3(object)
    
    
    if(ncol(betas) == 1){
      
      coefs2 <- sapply(1:ncol(re_2), function(x) {
        
        coefs <-  re_2[,x] + betas
        return(coefs)
      })
      
      coefs3 <- sapply(1:ncol(re_3), function(x) {
        
        coefs <-  re_3[,x] + betas
        return(coefs)
      })
      
      if(summary){ 
        coefs2 <- .summary_helper(coefs2, cred = cred)
        coefs3 <- .summary_helper(coefs3, cred = cred)
      }    
      
    
    } else {
    
      coefs2 <- .extract_re_2(object)
      coefs3 <- .extract_re_3(object)
      
      if(summary){
        coefs2 <- .summary_helper(coefs2, cred = cred)
        coefs3 <- .summary_helper(coefs3, cred = cred)
      }
      
      }
    
    coefs <- list()
    coefs[[1]] <- round(coefs2, digits = 3)
    coefs[[2]] <- round(coefs3, digits = 3)
    names(coefs) <- c("level_two", 
                      "level_three")
    
    }
  return(coefs)
}