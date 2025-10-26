
raking <- function(y, vars, sc, sp, wts.col, zcol, maxit=maxit, tol=tol) {
  
  # Design matrices
  Xc <- design_matrix(vars = vars, data = sc)
  Xp <- design_matrix(vars = vars, data = sp)
  beta <- rep(0, ncol(Xc))
  
  # Scale beta[1] to prevent singular matrix in first iteration
  f.p <- colSums(sp[,wts.col] * Xp)
  
  wts <- exp(as.vector(-Xc %*% beta))
  f.c <- colSums(wts * Xc)
  beta[1] <- -log(f.p[1] / f.c[1])
  
  # Newton-Raphson
  iter <- 0
  crit <- TRUE
  
  while (crit) {
    wts.sc <- as.vector(exp(-Xc %*% beta))
    
    f.c <- colSums(wts.sc * Xc)
    f <- f.c - f.p
    
    fprime <- -t(wts.sc * Xc) %*% Xc
    
    delta <- solve(fprime, f)
    beta <- beta - delta
    
    iter <- iter + 1
    crit <- iter < maxit && sum(abs(delta)) > tol
  }
  
  # Final weights
  wts.sc <- as.vector(exp(-Xc %*% beta))
  
  # Raking mean with domain variable 
  if (is.null(zcol)) Z = numeric(length(wts.sc))+1 else Z = sc[,zcol]
  Y  <- sc[[y]]*Z
  T1 <- sum(Y * wts.sc)
  T2 <- sum(Z * wts.sc)
  mu <- T1 / T2
  
  # Raking variance (matrix D by bootstrap will be updated, assume Poisson sampling for now)
  U_beta <- t(Y - mu*Z) %*% (wts.sc * Xc)
  S_beta <- t(wts.sc * Xc) %*% Xc
  b_vec  <- U_beta %*% solve(S_beta)
  
  D <- t(sp[,wts.col]^2 * (1-1/sp[,wts.col]) * Xp) %*% Xp
  
  v1 <- sum(wts.sc * (wts.sc - 1) * (Y - mu*Z - Xc %*% t(b_vec))^2)
  v2 <- b_vec %*% D %*% t(b_vec)
  v  <- as.vector((1 / T2^2) * (v1 + v2))
  
  return(list(wts_raking = wts.sc[Z], beta_raking = as.numeric(beta), 
              mean_raking = mu, variance_raking = v, names = colnames(Xc), iterations = iter))
}
