raking_nr <- function(
    Xc, 
    Xp_list, 
    wts_list, 
    maxit, 
    tol
) {
  # Xc       : sc design matrix (includes intercept column)
  # Xp_list  : list of m design matrices for each reference sample
  # wts_list : list of m weight vectors for each reference sample
  
  m <- length(Xp_list)    # number of reference samples
  p <- ncol(Xc)           # number of total covariates
  
  # --- 1) Initialize beta to zero ---
  beta <- rep(0, p)
  
  # --- 2) Compute f.p = concatenated colSums(Xp_j * wts_j) ---
  f.p <- unlist(lapply(seq_len(m), function(j) {
    Xp  <- Xp_list[[j]]
    wts <- wts_list[[j]]
    colSums(Xp * wts)
  }))
  
  # --- 3) Initialize beta[1] ---
  wts_sc  <- exp(- as.vector(Xc %*% beta))
  f.c     <- colSums(wts_sc * Xc)
  beta[1] <- -log(mean(f.p / f.c))
  
  # --- 4) Newton–Raphson iterations ---
  for (iter in seq_len(maxit)) {
    # a) update study-sample weights
    wts_sc <- exp(- as.vector(Xc %*% beta))
    # b) compute calibrated moments
    f.c <- colSums(wts_sc * Xc)
    # c) moment discrepancy
    f <- f.c - f.p
    # d) Jacobian matrix of f.c w.r.t. beta
    fprime <- - t(wts_sc * Xc) %*% Xc
    # e) Newton step
    delta <- solve(fprime, f)
    beta  <- beta - delta
    
    # f) check convergence
    conv_crit <- sum(abs(delta))
    if (conv_crit < tol) {
      break
    }
  }
  
  if (iter == maxit && sum(abs(delta)) >= tol) {
    warning("Maximum iterations reached without full convergence.")
  }
  
  return(list(beta=beta,iter=iter))
}
