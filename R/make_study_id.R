#' @title  Make \code{study_id} Variable
#' 
#' @description \link[blsmeta]{blsmeta} requires a numeric vector 
#'               for the \code{study_id} variable create that variable from 
#'               a character vector (for example).
#' 
#' @param x character vector
#'
#' @return numeric \code{study_id}
#' 
#' @export
#'
#' @examples
#' # data
#' library(psymetadata)
#' 
#' make_study_id( gnambs2020$author )
make_study_id <- function(x){
  match(x, sort(unique(x)))
}