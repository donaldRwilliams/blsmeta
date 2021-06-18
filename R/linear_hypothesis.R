#' Test Linear Hypotheses for Posterior Samples
#' 
#' 
#' @param lin_comb A string specifying a linear combination of variables, 
#'                 or a list of variable names if using \code{contrast}.
#'
#' @param obj An object of class \code{blsmeta}
#'
#' @param cred The level for which a credible interval should be computed.
#'
#' @param rope Specify a ROPE. Optional.
#'
#' @param contrast A contrast matrix specifying which combinations to test. Optional.
#' 
#' @param sub_model character. Which sub-model for the hypotheses, with options
#'                  including \code{location} and \code{scale}. The defaul
#'                  is \code{NULL}, so either is required.
#' 
#' @param print_names logical. Should the parameter names be printed ?
#'                    This is useful for specifying the hypotheses,
#'                    as the names need to be exact.
#'
#' @return An object of class \code{linear_hypothesis}
#' 
#' @importFrom bayeslincom lin_comb
#' @export
#' 
#' @examples 
#' library(psymetadata)
#' 
#' fit <-  blsmeta(yi = yi, 
#'                 vi = vi, 
#'                 es_id = es_id,
#'                 study_id = study_id,
#'                 data = gnambs2020)
#'                 
#' linear_hypothesis(obj = fit, 
#'                   lin_comb = "scale3_Intercept > scale2_Intercept",
#'                   sub_model = "scale")
linear_hypothesis <- function(lin_comb, obj, 
                              cred = 0.95, 
                              sub_model = NULL,
                              print_names = FALSE,
                              rope = NULL, 
                              contrast = NULL) {
  
 
  if(is.null(sub_model)){
    stop("sub_model must be specified. options include location and scale.")
  }
  
  if(isFALSE(sub_model %in% c("location", "scale"))){
    stop("sub_model not found. options include location and scale.")
  }
  
  
  samps <- posterior_samples(obj)
  colnames(samps ) <- gsub("[()]", "", colnames(samps))
  if(sub_model == "location"){
    samps <- samps[,grep("b_", x = colnames(samps)), drop = FALSE]
    
    colnames(samps) <- gsub(pattern = "b_", 
                           x = colnames(samps), 
                           replacement = "")
  } else if (sub_model == "scale"){
    
    samps2 <- samps[,grep("scale2_", x = colnames(samps)), 
                   drop = FALSE]
    
    samps3 <- samps[,grep("scale3_", x = colnames(samps)), drop = FALSE]
    
    samps <- cbind.data.frame(samps2, samps3)
  } else {
    
    }
  
  if(isTRUE(print_names)){
    print(colnames(samps))
  } else {
    
    out <- bayeslincom::lin_comb(
      lin_comb = lin_comb,
      obj = samps,
      ci = cred,
      rope = rope,
      contrast = contrast
    )
  out <- unclass(out)
  class(out) <- "linear_hypothesis"
  return(out)
}
}


#' Print formatted summary of a \code{linear_hypothesis} object
#'
#' @param x An object of class \code{linear_hypothesis}
#' @param ... Other arguments to be passed to \code{print}
#' @return A formatted summary of posterior samples
#' @export print.linear_hypothesis
#' @export
print.linear_hypothesis <- function(x, ...) {
  res <- x$results
  
  cri_raw <- extract_list_items(res, "ci")
  cri <- round(cri_raw, 2)
  
  Post.mean_raw <- extract_list_items(res, "mean_samples")
  Post.mean <- round(Post.mean_raw, 2)
  
  Post.sd_raw <- extract_list_items(res, "sd_samples")
  Post.sd <- round(Post.sd_raw, 2)
  
  print_df <- data.frame(
    Post.mean = Post.mean,
    Post.sd = Post.sd,
    Cred.lb = cri[1, ],
    Cred.ub = cri[2, ]
  )
  row.names(print_df) <- names(x$results)
  
  # ---- Begin pasting output ----
  # cat("blsmeta: Linear Hypothesis of Posterior Samples\n")
  # cat("------ \n")
  # cat("Call:\n")
  # print(x$call)
  # 
  # cat("------ \n")
  
  cat("Hypotheses:\n")
  comb_list <- extract_list_items(res, "lin_comb")
  
  for (comb in seq_along(comb_list)) {
    cat(paste0(" C", comb, ":"), comb_list[[comb]], "\n")
  }
  cat("------ \n")
  
  cat("Posterior Summary:\n\n")
  
  if (!is.null(x$rope)) {
    cat("ROPE: [", x$rope[[1]], ",", x$rope[[2]], "] \n\n")
    print_df$Pr.in <- extract_list_items(res, "rope_overlap")
    
    # note for ROPE
    note <- "Pr.in: Posterior probability in ROPE"
  } else {
    prob_greater <- extract_list_items(res, "prob_greater")
    print_df$Pr.less <- round(1 - prob_greater, 2)
    print_df$Pr.greater<- round(prob_greater, 2)
    
    note <- paste0("Pr.less: Posterior probability less than zero\n",
                   "Pr.greater: Posterior probability greater than zero")
  }
  
  print(print_df, right = T)
  cat("------ \n")
  cat(paste0("Note:\n", note))
}