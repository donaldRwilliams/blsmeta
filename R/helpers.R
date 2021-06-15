#' @importFrom stats density model.matrix pnorm qnorm terms coef rnorm
#' @importFrom methods is
#' 
#' @importFrom nlme fixef
#' @importFrom nlme ranef 
#' @export ranef 
#' @export fixef
two_level_rjags <- "
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
  fe_mu[i] <- inprod(X_location[i, ], beta) 
}

for (i in 1:K) {
    
    # predict tau
    tau_2[i] <- inprod(X_scale_2[i, ], gamma)
    
    # standard normal
    std_norm_2[i] ~ dnorm(0, 1)
    
    # level 2 random effects
    re_2[i] <- std_norm_2[i] * exp(tau_2[i])
    
  }

"

three_level_rjags <- "
for (i in 1:K) {
  # calculate precision
  prec[i] <- 1 / v[i]
  
  # mean
  mu[i] <-  fe_mu[i] + re_2[es_id[i]] + re_3[study_id[i]]
  
  # likelihood
  y[i] ~ dnorm(mu[i], prec[i])
  
}

# fixed effect
for (i in  1:K) {
  fe_mu[i] <- inprod(X_location[i, ], beta) 
}

for (i in 1:K) {
    
    # predict tau
    tau_2[i] <- inprod(X_scale_2[i, ], gamma)
    
    # standard normal
    std_norm_2[i] ~ dnorm(0, 1)
    
    # level 2 random effects
    re_2[i] <- std_norm_2[i] * exp(tau_2[i])
    
}

for (i in 1:J) {
    
    # predict tau
    tau_3[i] <- inprod(X_scale_3[i, ], eta)
    
    # standard normal
    std_norm_3[i] ~ dnorm(0, 1)
    
    # level 3 random effects
    re_3[i] <- std_norm_3[i] * exp(tau_3[i])
    
}

"

fe_rjags <- "
for (i in 1:K) {
  # calculate precision
  prec[i] <- 1 / v[i]
  
  # mean
  mu[i] <-  fe_mu[i] 
  
  # likelihood
  y[i] ~ dnorm(mu[i], prec[i])
  
}

# fixed effect
for (i in  1:K) {
  fe_mu[i] <- inprod(X_location[i, ], beta) 
}

"

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
  
  for(i in 1:K){
    ynew[i] ~ dnorm(fe_mu[i], 1/exp(tau_2[i])^2 )
  }
  
}

data_helper <- function(data, arg){
  
  # data frame
  data <- data.frame(data)
  
  # effect size
  yi <- as.numeric(eval(arg[[match("yi", names(arg))]], envir = data))
  
  # variances
  vi <- as.numeric(eval(arg[[match("vi", names(arg))]], envir = data))
  
  if(length(vi)==0){
   vi <- as.numeric(eval(arg[[match("sei", names(arg))]], envir = data))^2
  } 
  
  es_id <- as.numeric(eval(arg[[match("es_id", names(arg))]], envir = data))
  es_id <- 1:length(es_id)
  
  
  study_id <- as.numeric(eval(arg[[match("study_id", names(arg))]], envir = data))
  
  if(!is.numeric(study_id)){
    stop("study_id must be numeric")
  }
  
  study_id <- match(study_id, sort(unique(study_id)))
  
  
  list(y = yi, 
       v = vi, 
       es_id = es_id, 
       study_id = study_id)
}

prior_gamma_helper <- function(X2){
  
  # number of predictors
  p_gamma <- ncol(X2)
  
  # prior mean
  prior_mean_gamma <- c(-2, rep(0, p_gamma-1))
  
  # prior sd
  prior_sd_gamma <- c(1, rep(1, p_gamma-1))
  
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
      coda::as.mcmc(x$posterior_samples)
    )
  return(posterior_samples)
}

estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

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

extract_eta <- function(x, mean_X2){
  # x: posterior samples
  etas <-
    as.matrix(
      x[, grep("eta", colnames(x))]
    )
  
  # if intercept in model
  if(!any(is.na(mean_X2))){
    etas[,1] <- etas[,1] - t(mean_X2[-1] %*% t(etas[,-1]))
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
  
  betas <- betas[,paste0("beta[", 1:ncol(betas), "]")]
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
  
  x_new <- cbind(x[,1], scale(x[,-1], scale = F))
  
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

# I2 helper
I2_helper <- function(tau2, vi, method = "ht") {
  # number of studies
  k <- length(vi)
  if (method %in% c("ht", "rl", "rm")) {
    s2 <- s2_helper(vi = vi, method = method)
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

# cred helper
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
  
  return(eb_dat)
}

wi_star <- function(yi, vi, tau2i){
  wi <- 1/ vi
  wi_star <- 1/(1/wi + tau2i)
  return(wi_star)
}

re_mu <- function(yi, wi_star){
  re_mu <- sum(wi_star * yi) / sum(wi_star)
  return(re_mu)
}

re_mu_var <- function(wi_star){
  re_mu_var <- sqrt(1/sum(wi_star))
  return(re_mu_var)
}  

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

pwr_lsmeta <- function(true_effect, n, 
                       tau2, type = "r", alpha){
  vi <- 1/ (n - 3)
  if(type == "r"){
    z <- tanh(true_effect)
  }
  
  wistar <- wi_star(vi = vi, tau2i = tau2)
  zscore <- (z/sqrt(1/ sum(wistar)))
  zcr = qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  
  
  pwr <- 1 - pnorm(zcr - zscore)
  
  return(pwr)
  
}

scale_level_three_prior <- function(prior, X_scale_3){
  
  mm_mod_names <- colnames(X_scale_3)
  
  p <- ncol(X_scale_3)
  
  params <- sapply(prior, "[[", "param")
  
  # default priors
  if (length(params) == 0) {
    
    if(p == 1){
      
      if (all(X_scale_3[, 1] == 1)){
        
        default_prior <-
          paste0("#",
                 mm_mod_names[1],
                 "\neta[1] ~ dnorm(",-2,
                 ", 1)\n")
        
      } else {
        
        default_prior <-
          paste0("#", mm_mod_names[1], "\neta[1] ~ dnorm(", -2, ", 1)\n")
        
      }
      # end p
    } else {
      
      if(all(X_scale_3[, 1] == 1)){
        
        default_prior <-
          paste0("#",
                 mm_mod_names[1],
                 "\neta[1] ~ dnorm(",-2,
                 ", 1)\n")
        
        default_coef <-
          paste0("\n#",
                 mm_mod_names[-1],
                 "\n",
                 "eta[",
                 2:p,
                 "] ~ dnorm(0, 1)",
                 collapse = "\n")
        
        default_prior <- paste0("#Defaults\n",
                                default_prior,
                                default_coef)
        
      } else {
        
        default_prior <-
          paste0("\n#", mm_mod_names, 
                 "\neta[", 1:p, "] ~ dnorm(", -2, ", 1)", 
                 collapse = "\n")
      }
      
    }
    # end default
  }   else {
    
    if(!all( params %in% mm_mod_names)){
      stop("parameter not found in 'scale'")
    }
    
    user <- which(mm_mod_names %in% sapply(prior, "[[", "param"))
    
    default <- (1:p)[-user]
    
    if(default[1] == 1){
      
      default <- default[-1]
      
      if (all(X_scale_3[, 1] == 1)) {
        
        default_prior <-
          paste0("#",
                 mm_mod_names[1],
                 "\neta[1] ~ dnorm(",
                 -2,
                 ", 1)\n")
      } else {
        
        default_prior <-
          paste0("#", mm_mod_names[1], 
                 "\neta[1] ~ dnorm(", 0, 
                 ", 0.001)\n")
      }
      
      if(length(default) > 1){
        
        default_coef <- paste0("\n#",
                               mm_mod_names[default],
                               "\n",
                               "eta[",
                               default,
                               "] ~ dnorm(0, 1)",
                               collapse = "\n")
        
        default_prior <- paste0(default_prior, default_coef, "\n")
      }
      
      
    } else {
      
      default_coef <- paste0("\n#",
                             mm_mod_names[default],
                             "\n",
                             "eta[",
                             default,
                             "] ~ dnorm(0, 1)",
                             collapse = "\n")
      
    }
    
    
    user_coef <- paste0(
      "\n#",
      params,
      "\neta[",
      match(params,
            mm_mod_names),
      "]",
      " ~ ",
      sapply(prior, "[[", "prior"),
      collapse = "\n")
    
    default_prior <-  paste0("#Defaults\n",
                             default_prior,
                             "\n#Custom",
                             user_coef)
    
  }
  return(default_prior)
}


scale_level_two_prior <- function(prior, X_scale_2){
  
  mm_mod_names <- colnames(X_scale_2)
  
  p <- ncol(X_scale_2)
  
  params <- sapply(prior, "[[", "param")
  
  # default priors
  if (length(params) == 0) {
    
    if(p == 1){
      
      if (all(X_scale_2[, 1] == 1)){
        
        default_prior <-
          paste0("#",
                 mm_mod_names[1],
                 "\ngamma[1] ~ dnorm(",-2,
                 ", 1)\n")
        
      } else {
        
        default_prior <-
          paste0("#", mm_mod_names[1], "\ngamma[1] ~ dnorm(", -2, ", 1)\n")
        
        }
    # end p
    } else {
      
      if(all(X_scale_2[, 1] == 1)){
      
        default_prior <-
          paste0("#",
               mm_mod_names[1],
               "\ngamma[1] ~ dnorm(",-2,
               ", 1)\n")
      
        default_coef <-
          paste0("\n#",
                 mm_mod_names[-1],
                 "\n",
                 "gamma[",
                 2:p,
                 "] ~ dnorm(0, 1)",
                 collapse = "\n")

        default_prior <- paste0("#Defaults\n",
                                default_prior,
                                default_coef)
        
      } else {
      
        default_prior <-
          paste0("\n#", mm_mod_names, "\ngamma[", 1:p, "] ~ dnorm(", -2, ", 1)",collapse = "\n")
    }

    }
  # end default
  }   else {

    if(!all( params %in% mm_mod_names)){
      stop("parameter not found in 'scale'")
    }
    
    user <- which(mm_mod_names %in% sapply(prior, "[[", "param"))
    
    default <- (1:p)[-user]
    
    if(default[1] == 1){
      
      default <- default[-1]
      
      if (all(X_scale_2[, 1] == 1)) {

              default_prior <-
                paste0("#",
                       mm_mod_names[1],
                       "\ngamma[1] ~ dnorm(",
                       -2,
                       ", 1)\n")
            } else {
              
              default_prior <-
                paste0("#", mm_mod_names[1], "\ngamma[1] ~ dnorm(", 0, ", 0.001)\n")
        }
        
      if(length(default) > 1){
        
        default_coef <- paste0("\n#",
                               mm_mod_names[default],
                               "\n",
                               "gamma[",
                               default,
                               "] ~ dnorm(0, 1)",
                               collapse = "\n")
        
        default_prior <- paste0(default_prior, default_coef, "\n")
        }
    
    
    } else {
      
      default_coef <- paste0("\n#",
                             mm_mod_names[default],
                             "\n",
                             "gamma[",
                             default,
                             "] ~ dnorm(0, 1)",
                             collapse = "\n")
      
    }
    
    
        user_coef <- paste0(
          "\n#",
          params,
          "\ngamma[",
          match(params,
                mm_mod_names),
          "]",
          " ~ ",
          sapply(prior, "[[", "prior"),
          collapse = "\n")
    
    default_prior <-  paste0("#Defaults\n",
                                 default_prior,
                                 "\n#Custom",
                                   user_coef)
   
  }
  return(default_prior)
}

location_prior <- function(prior, X_location, yi){
  
  mm_mod_names <- colnames(X_location)
  
  p <- ncol(X_location)
  
  params <- sapply(prior, "[[", "param")
  
  # default priors
  if (length(params) == 0) {
    
    if (all(X_location[, 1] == 1)) {
      
      default_prior <-
        paste0("#",
               mm_mod_names[1],
               "\nbeta[1] ~ dnorm(",
               round(mean(yi), 3),
               ", 5)\n")
    } else {
      
      default_prior <-
        paste0("#", mm_mod_names[1], "\nbeta[1] ~ dnorm(", 0, ", 0.001)\n")
    }
    
    if (p > 1) {
      
      default_coef <-
        paste0("\n#",
               mm_mod_names[-1],
               "\n",
               "beta[",
               2:p,
               "] ~ dnorm(0, 0.001)",
               collapse = "\n")
      
      default_prior <- paste0("#Defaults\n", 
                              default_prior, 
                              default_coef)
      
    }
  } else {
    
    if(!all( params %in% mm_mod_names)){
      stop("parameter not found in 'location'")
    }
    
    user <- which(mm_mod_names %in% sapply(prior, "[[", "param"))
    
    default <- (1:p)[-user]
    
    if(length(user) == 1){
      
      default_prior <-
        paste0("#User\n", "beta[1] ~ ", sapply(prior, "[[", "prior"))
      
    } else {
      
      if (default[1] == 1) {
        
        default <- default[-1]
        
        if (all(X_location[, 1] == 1)) {
          
          default_int <-
            paste0("#",
                   mm_mod_names[1],
                   "\nbeta[1] ~ dnorm(",
                   round(mean(yi), 3),
                   ", 1)\n")
        } else {
          
          default_int <-
            paste0("#", mm_mod_names[1], "\nbeta[1] ~ dnorm(", 0, ", 0.001)\n")
        }
      } else {
        
        default_int <- ""
      } 
      if (p > 1) {
        
        default_coef <- paste0("\n#",
                               mm_mod_names[default],
                               "\n",
                               "beta[",
                               default,
                               "] ~ dnorm(0, 0.001)",
                               collapse = "\n")
        
        params <- sapply(prior, "[[", "param")
        
        user_coef <- paste0(
          "\n#",
          params,
          "\nbeta[",
          match(params,
                mm_mod_names),
          "]",
          " ~ ",
          sapply(prior, "[[", "prior"),
          collapse = "\n"
        )
        
        default_prior <-  paste0("#Defaults\n",
                                 default_int,
                                 default_coef,
                                 "\n\n#Custom",
                                 user_coef)
      }
    }
  }
  return(default_prior)
}

set_prior <- function(param, prior, dpar, level = NULL){
  ls <- list(list(param = param , prior = prior, level = level))
  names(ls) <- dpar
  ls
}

.extract_samples <- function(object) {
  samps <- object$posterior_samples
  
  samps <- do.call(rbind.data.frame,
                   lapply(seq_len(object$chains), function (x) {
                     as.data.frame(samps[[x]][-c(1:object$warmup), ])
                   }))
  if (ncol(samps) == 1) {
    colnames(samps)[1] <- "beta[1]"
  }
  return(samps)
}

.extract_beta <- function(object){
  x <- .extract_samples(object)
  betas <- 
    as.matrix(
      x[, grep("beta", colnames(x))]
    )
  if(ncol(betas) > 1 & all(object$X_location[, 1] == 1)) {
    betas[, 1] <- betas[, 1] - t(object$X_location_means[-1] %*% t(betas[, -1]))
  }
  return(betas)
}

.extract_gamma <- function(object){
  x <- .extract_samples(object)
  gammas <- 
    as.matrix(
      x[, grep("gamma", colnames(x))]
    )
  if(ncol(gammas) > 1 & all(object$X_scale2[, 1] == 1)) {
    gammas[, 1] <- gammas[, 1] - t(object$X_scale2_means[-1] %*% t(gammas[, -1]))
  }
  return(gammas)
}


.extract_eta <- function(object){
  x <- .extract_samples(object)
  p <- ncol(object$X_scale3)
  
  if(p == 1){ 
  etas <- 
    as.matrix(
      x[, grep("eta", colnames(x))][,2]
    )
  } else {
    etas <- 
      as.matrix(
        x[, paste0("eta[",1:p, "]")]
      )
    
  }
 
  if(ncol(etas) > 1 & all(object$X_scale3[, 1] == 1)) {
    etas[, 1] <- etas[, 1] - t(object$X_scale3_means[-1] %*% t(etas[, -1]))
  }
  return(etas)
}

.extract_scale2 <- function(object){
  x <- .extract_samples(object)
  tau_2 <- x[, grep("tau_2", colnames(x))]
  return(tau_2)
}

.extract_scale3 <- function(object){
  x <- .extract_samples(object)
  tau_3 <- x[, grep("tau_3", colnames(x))]
  return(tau_3)
}

check_level_3 <- function(x, dat_check){
  ids <- unique(dat_check$study_id_new)
  preds <- labels(terms(x))
  
  unlist(
    lapply(seq_along(preds), function(z){
      lapply(ids, function(x) {
        length((unique( subset(dat_check, 
                               study_id_new == ids[x], 
                               select = preds[1])))[,1])
      })
      
    }
    ))
  
}

.summary_helper <- function(x, cred){
  lb <- (1 - cred) / 2
  ub <- 1 - lb
  
  dat <- data.frame(
    Post.mean = apply(x, 2, mean),
    Post.sd = apply(x, 2, sd),
    Cred.lb = apply(x, 2,  quantile, probs = lb),
    Cred.ub = apply(x, 2,  quantile, probs = ub)
  )
  
  row.names(dat) <- 1:ncol(x)
  
  return(dat)
  
}

.extract_re_2 <- function(object){
  
  x <- .extract_samples(object)
  
  re_2 <- as.matrix(
    x[, grep("re_2", colnames(x) )]
  )
  return(re_2)
}


.extract_re_3 <- function(object){
  
  x <- .extract_samples(object)
  
  re_3 <- as.matrix(
    x[, grep("re_3", colnames(x) )]
  )
  return(re_3)
}


globalVariables(c("K","X", 
                  "X2", "es_id", 
                  "etas", "gammas", 
                  "inprod", "p_gamma", 
                  "std_norm_2", 
                  "k_per_study",
                  "study_id_new",
                  "p_beta",
                  "v", "vi", 
                  "yi", "p"))

