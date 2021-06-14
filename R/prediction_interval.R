prediction_interval <- function(object){
  if (!is(object, "blsmeta")) {
    stop("object must be of class 'blsmeta'")
  }
  
  if(object$model == "fe"){
    stop("Fixed-effects models not supported. see 'fitted'.")
  }
  
  
}