#' @title  Extract Posterior Samples
#' 
#' @description  Extract posterior samples from an 
#'               object of class \code{blsmeta}
#' 
#' @param object An object of class \code{blsmeta}.
#' 
#' @param param character string. The parameter(s) to be extracted.
#'              See \strong{Details}
#'              
#' @param print_names logical. Should the parameter names be printed 
#'                    (defaults to \code{FALSE})? This is useful
#'                    for seeing which parameters can be extracted.            
#'
#' @return A \code{iter} * \code{chains}  by parameter matrix of posterior
#'         samples
#' 
#' @details Options for \code{param} depend on the model, but can included
#'          printed out by setting \code{print_names = TRUE}.
#'
#' Note the variance components, and log-linear coefficients are on 
#' the standard deviation scale.                    
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
#' posterior_samples(object = fit, 
#'                   print_names = TRUE)
posterior_samples <- function(object, 
                              param = NULL, 
                              print_names  = FALSE){
  if(object$model == "fe"){
    betas <- .extract_beta(object)
    colnames(betas) <- paste0("b_", colnames(object$X_location))
  } else if (object$model == "two_level"){
    betas <- .extract_beta(object)
    colnames(betas) <- paste0("b_", colnames(object$X_location))
    
    gammas <- .extract_gamma(object)
    colnames(gammas) <- paste0("scale2_", colnames(object$X_scale2))
    
    re_2 <- .extract_re_2(object)
    colnames(re_2) <- paste0("es_id[", 1:ncol(re_2), "]")  
    
    tau_2 <- .extract_scale2(object)
   
    samps <- cbind.data.frame(betas, gammas, re_2, tau_2) 
  } else {
    
    betas <- .extract_beta(object)
    colnames(betas) <- paste0("b_", colnames(object$X_location))
    
    gammas <- .extract_gamma(object)
    colnames(gammas) <- paste0("scale2_", colnames(object$X_scale2))
    
    etas <- .extract_eta(object)
    colnames(etas) <- paste0("scale3_", colnames(object$X_scale3))
    
    re_2 <- .extract_re_2(object)
    colnames(re_2) <- paste0("es_id[", 1:ncol(re_2), "]")  
    
    re_3 <- .extract_re_3(object)
    colnames(re_3) <- paste0("study_id[", 1:ncol(re_3), "]")  
    
    tau_3 <- .extract_scale3(object)
    tau_2 <- .extract_scale2(object)
    
    samps <- cbind.data.frame(betas, gammas, 
                              etas, re_2, 
                              tau_2, re_3, tau_3) 
    
    
  }
  
  if(print_names){
    print(colnames(.extract_samples(object)))
  } else {
  
  
  if(!is.null(param)){
    samps <- as.matrix(samps[, grep(param, colnames(samps)) ])
    if(length(samps) == 0){
      stop("param not found. try 'print_names = TRUE'")
    }
  }
  
  return(samps)

  }
}