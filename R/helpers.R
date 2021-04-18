#' @importFrom nlme ranef
#' @export ranef
two_level <- function() {
  
  for (i in 1:K) {
    # calculate precision
    prec[i] <- 1 / v[i]
    
    # mean
    mu[i] <-  fe_mu[i] + re_2[es_id[i]]
    
    # likelihood
    y[i] ~ dnorm(mu[i], prec[i])
    
  }
  
  # fixed effect
  for (i in  1:K) {
    fe_mu[i] <- inprod(X[i, ], beta) 
  }
  
  # priors for beta
  for (i in 1:p_beta) {
    beta[i] ~ dnorm(prior_mean_beta[i],
                    pow(prior_sd_beta[i],-2))
    
  }
  
  # level 2 sd
  for (i in 1:K) {
    
    # predict tau
    tau_2[i] <- inprod(X2[i, ], gamma)
    
    # standard normal
    std_norm_2[i] ~ dnorm(0, 1)
    
    # level 2 random effects
    re_2[i] <- std_norm_2[i] * exp(tau_2[i])
    
  }
  
  # prior level 2 coefficients
  for (i in 1:p_gamma) {
    
    gamma[i] ~ dnorm(prior_mean_gamma[i],
                     pow(prior_sd_gamma[i],-2))
    
  }
  
}

data_helper <- function(data, arg){
  
  # data frame
  data <- data.frame(data)
  
  # effect size
  yi <- as.numeric(eval(arg[[match("yi", names(arg))]], envir = data))
  
  # variances
  vi <- as.numeric(eval(arg[[match("vi", names(arg))]], envir = data))
  
  es_id <- as.numeric(eval(arg[[match("es_id", names(arg))]], envir = data))
  
  
  list(y = yi, 
       v = vi, 
       es_id = es_id)
}

prior_gamma_helper <- function(X2){
  
  # number of predictors
  p_gamma <- ncol(X2)
  
  # prior mean
  prior_mean_gamma <- c(-2, rep(0, p_gamma-1))
  
  # prior sd
  prior_sd_gamma <- rep(1, p_gamma)
  
  list(p_gamma = p_gamma, 
       prior_mean_gamma = prior_mean_gamma, 
       prior_sd_gamma =  prior_sd_gamma)
  
}

prior_beta_helper <- function(X, mu){
  
  # number of predictors
  p_beta <- ncol(X)
  
  # prior mean
  prior_mean_beta <- c(mu, rep(0, p_beta-1))
  
  # prior sd
  prior_sd_beta <- rep(5, p_beta)
  
  list(p_beta = p_beta, 
       prior_mean_beta = prior_mean_beta, 
       prior_sd_beta =  prior_sd_beta)
  
}

extract_samples <- function(x){
  # x: fitted model
  posterior_samples <- 
    do.call(
      rbind.data.frame, 
      coda::as.mcmc(x)
    )
  return(posterior_samples)
}

# extract_samples <- function (x, ...) {
#   UseMethod("extract_samples", x)
# }

extract_gamma <- function(x, mean_X2){
  # x: posterior samples
  gammas <-
    as.matrix(
      x[, grep("gamma", colnames(x))]
    )
  
  # if intercept in model
  if(!any(is.na(mean_X2))){
    gammas[,1] <- gammas[,1] - t(mean_X2[-1] %*% t(gammas[,-1]))
  }
  return(gammas)
}

extract_beta <- function(x, mean_X){
  # x: posterior samples
  betas <- 
    as.matrix(
      x[, grep("beta", colnames(x))]
    )
  if(!any(is.na(mean_X))){
    betas[,1] <- betas[,1] - t(mean_X[-1] %*% t(betas[,-1]))
  }
  
  return(betas)
}

extract_re_2 <- function(x){
  # x: posterior samples
  re_2 <- 
    as.matrix(
      x[, grep("re_2", colnames(x))]
    )
  # order
  re_2 <- re_2[,paste0("re_2[", 1:ncol(re_2), "]")]
  return(re_2)
}

center_helper <- function(x){
  
  unique_cols <- apply(x, 2, function(i) {
    length(unique(i))
  })
  
  if (length(unique_cols) == 1) {
    x_new <- x
    
  } else {
    if (unique_cols[1] == 1) {
      x_new <- as.matrix(x[,-1])
      
      for (i in seq_along(ncol(x_new))) {
        x_new[, i] <- x_new[, i] - mean(x_new)
        
      }
      
      x_new <- cbind(1, x_new)
    } else {
      x_mean <- NA
      
    }
    
  }
  
  colnames(x_new) <- colnames(x)
  
  list(x  = x_new,
       x_mean = colMeans(x),
       xold = x)
}

# s2 helper
s2_helper <- function(vi, method = "ht"){
  # number of studies
  k <- length(vi)
  # weights
  wi <- 1/vi
  # Equation 9 in
  # Higgins, J. P., & Thompson, S. G. (2002). Quantifying 
  # heterogeneity in a meta‐analysis. Statistics in medicine,
  # 21(11), 1539-1558.
  if(method == "ht"){
    s2 <- sum(wi * (k - 1)) / (sum(wi)^2 - sum(wi^2))
  } else if (method == "rl"){
    s2 <- k / sum(sqrt(vi)^-2)
  } else if (method == "rm"){
    s2 <- mean(vi)
  } else {
    stop("method not supported. must be 'ht', 'rl', or 'rm'.")
  }
  return(s2)
}

I2_helper <- function(tau2, vi, method = "ht") {
  # number of studies
  k <- length(vi)
  if (method %in% c("ht", "rl", "rm")) {
    s2 <- s2_helper(vari = vi, method = method)
    I2 <- tau2 / (tau2 + s2)
  } else {
    I2 <- mean(tau2 / (tau2 + vi))
  }
  return(I2)
}

# kl divergence
kl <- function(v1, v2, d1, d2){
  (log(sqrt(v2)/ sqrt(v1)) + ((v1 + (d1 - d2)^2)/(2*v2))) - 0.5
}

cred_helper <- function(x){
  lb <- (1 - x) / 2
  ub <- 1 - lb
  return(c(lb, ub))
}

plug_in_sf <- function(vi, tau2){
  sf <- tau2 / (tau2 + vi)
  return(sf)
}

plug_in_eb <- function(overall_mean, 
                       study_mean, 
                       vi, 
                       tau2){
  
  sf <- plug_in_sf(vi, tau2)
  
  eb <- (sf * study_mean)  + ((1 - sf) * overall_mean)
  
  eb_var <- sqrt(vi) * sqrt(sf)
  
  eb_dat <- data.frame(eb = eb, eb_var = eb_var)
  
  return(eb_data)
}
