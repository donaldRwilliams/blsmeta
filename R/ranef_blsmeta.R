#' @title Extract Group-Level Estimates
#' 
#' @name ranef
#' 
#' @description Extract the group-level ("random") effects from 
#' a \code{blsmeta} object.
#'
#' @param object An object of class \code{blsmeta}
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
#' dat <- gnambs2020
#' 
#' fit <- blsmeta(yi = yi, 
#'                vi = vi, 
#'                es_id = es_id, 
#'                data = dat)
#' 
#' ranef(fit)
ranef.blsmeta <- function(object, 
                          summary = TRUE, 
                          cred = 0.95, 
                          digits = 3, ...){
  
  if (object$model == "fe") {
    stop("fixed-effects models not supported")
  }
  
  if(object$model == "two_level"){
    res <- list()
    res2 <- .extract_re_2(object)
    
    if(summary){
      res2 <- round(
        .summary_helper(res2, cred = cred), 
        digits = digits)
    }
    
    res[[1]] <- res2
    names(res) <- "level_two"
    
  } else {
    res <- list()
    res2 <- .extract_re_2(object)
    res3 <- .extract_re_3(object)
    
    if(summary){
      
      res2 <- round(
        .summary_helper(res2, cred = cred), 
        digits = digits)
      
      res3 <- round(
        .summary_helper(res3, cred = cred), 
        digits = digits)
      
      res[[1]] <- res2
      res[[2]] <- res3
      
      
    } else {
      res[[1]] <- res2
      res[[2]] <- res3
    } 
    
    
    names(res) <- c("level_two", "level_three")
  }
  
  
  returned_object <- list(ranefs = res)
  class(returned_object) <- c("blsmeta", "ranef")
  return(returned_object)
}
