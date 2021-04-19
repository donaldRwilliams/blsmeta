#' Title
#'
#' @param object 
#' @param summary 
#' @param cred 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
coef.blsmeta <- function(object, summary = TRUE, cred = 0.95, ...){
  
  samps <- extract_samples(object)
  
  re2 <- extract_re_2(samps)
  
  int <- as.matrix(extract_beta(samps, object$mean_X)[,1])
  
  k <- ncol(re2)
  
  colnames(re2) <- 1:k
  
  coefs <- sapply(1:k, function(x) {
    coef <- (int + re2[,x])
    return(coef)
    })
  
  if (summary) {
    creds <- cred_helper(cred)
    coefs <- data.frame(
      Post.mean = colMeans(coefs),
      Post.sd = apply(coefs, 2, sd),
      Cred.lb = apply(coefs, 2,  quantile, creds[1]),
      Cred.ub = apply(coefs, 2,  quantile, creds[2])
    )
  }
  
  return(coefs)
}