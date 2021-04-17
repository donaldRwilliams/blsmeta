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
ranef.blsmeta <- function(object, summary = TRUE, cred = 0.95, ...){
  
  samps <- extract_samples(object)
  
  re2 <- extract_re_2(samps)
  
  k <- ncol(re2)
  
  colnames(re2) <- 1:k
  
  if (summary) {
    creds <- cred_helper(cred)
    re2 <- data.frame(
      Post.mean = colMeans(re2),
      Post.sd = apply(re2, 2, sd),
      Cred.lb = apply(re2, 2,  quantile, creds[1]),
      Cred.ub = apply(re2, 2,  quantile, creds[2])
    )
  }
  
  return(re2)
}