fitted.blsmeta <- function(object, 
                           newdata = NULL, 
                           newdata_scale2 = NULL, 
                           newdata_scale3 = NULL, 
                           re_formula = NULL,
                           summary = TRUE, 
                           cred = 0.95, 
                           digits = 3, 
                           ...){
  
  if(object$model == "fe"){
    
    if(is.null(newdata)){
      
      pred <- t(object$X_location_old %*% t(.extract_beta(object)))
      
    } else {
      
      pred <- t(model.matrix(object$mods_f, newdata) %*% t(.extract_beta(object)))
      
    }
    
    # end fe  
  } else if (object$model == "")
    
    if(summary){
      pred <- round(
        .summary_helper(pred, cred = cred), 
        digits = digits)
    }
  
  return(pred)
}
