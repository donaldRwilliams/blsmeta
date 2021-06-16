#' @title  Get JAGS Code from Fitted Model
#'
#' @param object An object of class \code{blsmeta}
#'
#' @return JAGS code printed out.
#' @export
#'
#' @examples
#' library(psymetadata)
#' 
#' fit <-  blsmeta(yi = yi, 
#'                 vi = vi, 
#'                 mods = ~n,
#'                 es_id = es_id,
#'                 data = gnambs2020)
#'                 
#' get_model(object)
get_model <- function(object){
  cat(object$model_code)
}