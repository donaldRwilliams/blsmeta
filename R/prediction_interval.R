#' @title Posterior Predictive Distribution
#' 
#' @description Draw sample from the posterior predictive distribution.
#' 
#' @param object An object of class \code{blsmeta}.
#' 
#' @param newdata An optional data.frame for which to compute
#'                fit fitted values. Defaults to \code{NULL}, which then 
#'                uses the original data used in \code{\link[blsmeta]{blsmeta}}.
#' 
#' @param newdata_scale2 An optional data.frame for which to compute
#'                       predictions for the level 2 variance component. 
#'                       Defaults to \code{NULL}, which then uses the 
#'                       original data used in \code{\link[blsmeta]{blsmeta}}.
#' 
#' 
#' @param newdata_scale3 An optional data.frame for which to compute
#'                       predictions for the level 3 variance component. 
#'                       Defaults to \code{NULL}, which then uses the 
#'                       original data used in \code{\link[blsmeta]{blsmeta}}
#' 
#' @param summary  logical. Should the posterior samples be summarized 
#'                 (defaults to \code{TRUE})?
#' 
#' 
#' @param new_level logical. Should the predictive distribution be for 
#'                  new levels (defaults to \code{TRUE})? See \strong{Details}. 
#' 
#' @param cred numeric. credible interval (defaults to \code{0.95}).
#' 
#' @param digits numeric. The desired number of digits for the summarized 
#'               estimates (defaults to \code{3}).
#'
#' @param ... Currently ignored.
#' 
#' @details When \code{new_level = TRUE}, this provides the predictive 
#'          distribution with respect to the moderators for the sub-models.
#'          For obtaining the predictive distribution for the observed
#'          levels, set this to \code{FALSE} which then includes the 
#'          "random" effects.
#' 
#' @return A data frame of fitted values (when \code{summary = TRUE}), or 
#'         a matrix of the unsummarized posterior samples.
#' 
#' @export
#'
#' @examples
#' library(psymetadata)
#' 
#' fit <-  blsmeta(yi = yi, 
#'                 vi = vi, 
#'                 es_id = es_id,
#'                 data = gnambs2020)
#'                 
#' predict(fit)
predict <- function(object, 
                    newdata = NULL,
                    newdata_scale2 = NULL, 
                    newdata_scale3 = NULL, 
                    summary = TRUE, 
                    new_level = TRUE,
                    cred = 0.95, 
                    digits = 3, ...){
  if (!is(object, "blsmeta")) {
    stop("object must be of class 'blsmeta'")
  }
  
  if(object$model == "fe"){
    stop("Fixed-effects models not supported. see 'fitted'.")
  }
  
  if(object$model == "two_level"){
    
    
    if(new_level){
      
      if(is.null(newdata)){
        mm_l <- object$X_location_old
      
      } else {
        mm_l <- model.matrix(object$mods_f, newdata)
      }
      
      if(is.null(newdata_scale2)){
        mm_s <- object$X_scale2_old
       
      } else {
        mm_s <- model.matrix(object$mods_scale2_f, newdata_scale2)
      }
      
      
      betas <- t(mm_l %*% t(.extract_beta(object)))
      gammas <- exp(t(mm_s %*% t(.extract_gamma(object))))
      
      if(ncol(betas) != ncol(gammas)){
        stop("must have same number of rows in newdata and newdata_scale2.")
      }
      
      yrep <- sapply(1:ncol(betas), function(x) {
        rnorm(nrow(betas), betas[,x], gammas[,x])
        
      })
      
      # end new level
    } else {
      
      re_2 <- .extract_re_2(object)
      
      if(!is.null(newdata)){
        message("newdata ignored. see 'Details'")
      } 
      
      if(!is.null(newdata_scale2)){
        message("newdata_scale2 ignored. see 'Details'")
      }
      mm_l <- object$X_location_old
      mm_s <- object$X_scale2_old
      
      betas <- .extract_beta(object)
      gammas <- exp(t(mm_s %*% t(.extract_gamma(object))))
      
      betas_new <- sapply(1:ncol(re_2), function(x){
        cbind(betas[,1] + re_2[,x], betas[,-1])
      })
      
      yrep <- sapply(1:ncol(betas_new), function(x){  
        yrep <- rnorm(n = nrow(gammas), 
                      mean = betas_new[,x],  
                      sd = gammas[,x])
        return(yrep)
      })
    }
    # end two level
  } else {
    
    
    if(new_level){
      
      if(is.null(newdata)){
        mm_l <- object$X_location_old
        
      } else {
        mm_l <- model.matrix(object$mods_f, newdata)
      }
      
      if(is.null(newdata_scale2)){
        mm_s2 <- object$X_scale2_old
        
      } else {
        mm_s2 <- model.matrix(object$mods_scale2_f, newdata_scale2)
      }
      
      if(is.null(newdata_scale3)){
        mm_s3 <- object$X_scale3_old
        
        etas <- exp(t(mm_s3 %*% t(.extract_eta(object))))
        
        k_per_study <- tapply(1:object$k , 
                              object$dat_list$study_id, length)
        
        etas <- 
          do.call(cbind,
                  sapply(1:object$J, function(x){
                    replicate(k_per_study[x], etas[,x])
                  }))
      } else {
        mm_s3 <- model.matrix(object$mods_scale3_f, newdata_scale3)
        etas <- exp(t(mm_s3 %*% t(.extract_eta(object))))
      }
      
      betas <- t(mm_l %*% t(.extract_beta(object)))
      gammas <- exp(t(mm_s2 %*% t(.extract_gamma(object))))
      
      if(ncol(betas) != ncol(gammas) | ncol(betas) != ncol(etas)){
        stop("must have same number of rows in newdata and newdata_scale2.")
      }
      
      yrep <- sapply(1:ncol(betas), function(x) {
        rnorm(nrow(betas), betas[,x], sqrt(gammas[,x]^2 + etas[,x]^2))
        
      })
      
      
      
      # end new level
    } else {
      
      re_2 <- .extract_re_2(object)
      re_3 <- .extract_re_3(object)
      
      k_per_study <- tapply(1:object$k , 
                            object$dat_list$study_id, length)
      
      re_3 <- 
        do.call(cbind,
                sapply(1:object$J, function(x){
                  replicate(k_per_study[x], re_3[,x])
                }))
      
      
      if(!is.null(newdata)){
        message("newdata ignored. see 'Details'")
      } 
      
      if(!is.null(newdata_scale2)){
        message("newdata_scale2 ignored. see 'Details'")
      }
      
      if(!is.null(newdata_scale3)){
        message("newdata_scale2 ignored. see 'Details'")
      }
      mm_l <- object$X_location_old
      mm_s2 <- object$X_scale2_old
      mm_s3 <- object$X_scale3_old
      
      betas <- .extract_beta(object)
      gammas <- exp(t(mm_s2 %*% t(.extract_gamma(object))))
      etas <- exp(t(mm_s3 %*% t(.extract_eta(object))))
      
      etas <- 
        do.call(cbind,
                sapply(1:object$J, function(x){
                  replicate(k_per_study[x], etas[,x])
                }))
      
      betas_new <- sapply(1:ncol(re_2), function(x){
        cbind(betas[,1] + re_2[,x] + re_3[,x], betas[,-1])
      })
      
      yrep <- sapply(1:ncol(betas_new), function(x){  
        yrep <- rnorm(n = nrow(gammas), 
                      mean = betas_new[,x],  
                      sd = gammas[,x])
        return(yrep)
      })
    }
  } # end three level
  
  if(summary){
    yrep <- 
      round(
        .summary_helper(x = yrep, cred = cred),
        digits = digits)
  }
  return(yrep)
}
