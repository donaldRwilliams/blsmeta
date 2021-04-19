#' Title
#'
#' @param fit_re 
#' @param fit_me 
#' @param mod_tau_2 
#' @param data 
#' @param measure 
#' @param summary 
#'
#' @return
#' @export
#'
#' @examples
pseudo_R2 <- function(fit_re, 
                      fit_me, 
                      mod_tau_2 = NULL, 
                      summary = TRUE,
                      cred = 0.95,
                      data, measure = "r2"){
  
  # if(nrow(tau_re) != nrow(tau_me)){
  #   stop("different number of posterior samples")
  # }
  # 
  # if (!all(colnames(X2) == colnames(object$x2old))) {
  #   stop("differt mod_tau_2. must be the same in both models")
  # }
  
  if (measure == "r2") {
    
    tau_re <- tau(
      x = fit_re,
      mod_tau_2 = mod_tau_2,
      data = data,
      summary = TRUE
    )
    
    tau_me <- tau(
      x = fit_me,
      mod_tau_2 = mod_tau_2,
      data = data,
      summary = TRUE
    )
    
    out <-
      (tau_re$Post.mean ^ 2 - tau_me$Post.mean ^ 2) / (tau_re$Post.mean ^ 2)
    
    out <- data.frame(psuedo_R2 = out)
    
  } else if (measure == "diff") {
    tau_re <- tau(
      x = fit_re,
      mod_tau_2 = mod_tau_2,
      data = data,
      summary = FALSE
    )
    
    tau_me <- tau(
      x = fit_me,
      mod_tau_2 = mod_tau_2,
      data = data,
      summary = FALSE
    )
    
    out <- tau_me - tau_re
    
    if (summary) {
      creds <- cred_helper(cred)
      lb <- creds[1]
      ub <- creds[2]
      
      out <- data.frame(
        Post.mean = colMeans(out),
        Post.sd = apply(out, 2, sd),
        Cred.lb = apply(out, 2,  quantile, lb),
        Cred.ub = apply(out, 2,  quantile, ub)
      )
    }
    
  }
  return(out)
}


