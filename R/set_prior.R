#' Title
#'
#' @param param 
#' @param prior 
#' @param dpar 
#' @param level 
#'
#' @return
#' @export
#'
#' @examples
set_prior <- function(param, prior, dpar, level = NULL){
  ls <- list(list(param = param , prior = prior, level = level))
  names(ls) <- dpar
  ls
}