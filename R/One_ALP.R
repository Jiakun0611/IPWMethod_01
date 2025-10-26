ALP <- function(y, vars, sc, sp, wts.col, zcol, maxit=maxit, tol=tol) {
  # Design matrices
  Xc <- design_matrix(vars = vars, data = sc)
  Xp <- design_matrix(vars = vars, data = sp)
  
  # Newton-Raphson
  beta <- rep(0, ncol(Xc))
  p.sp <- as.vector(expit(Xp %*% beta))
  p.sc <- as.vector(expit(Xc %*% beta))
  
  # Scale beta[1] to prevent singular matrix in first iteration
  f.p <- colSums(sp[,wts.col] * p.sp * Xp)
  f.c <- colSums((1 - p.sc) * Xc)
  beta[1] <- -log(f.p[1] / f.c[1])
  
  iter <- 0
  crit <- TRUE
  
  while (crit) {
    p.sp <- as.vector(expit(Xp %*% beta))
    p.sc <- as.vector(expit(Xc %*% beta))
    
    f.p <- colSums(sp[,wts.col] * p.sp * Xp)
    f.c <- colSums((1 - p.sc) * Xc)
    f <- f.c - f.p
    
    fprime.p <- t(sp[,wts.col] * p.sp * (1 - p.sp) * Xp) %*% Xp
    fprime.c <- t(p.sc * (p.sc - 1) * Xc) %*% Xc
    fprime <- fprime.c - fprime.p
    
    delta <- solve(fprime, f)
    beta  <- beta - delta
    
    iter <- iter + 1
    crit <- iter < maxit && sum(abs(delta)) > tol
  }
  
  # Final weights
  p.sp <- as.vector(expit(Xp %*% beta))
  p.sc <- as.vector(expit(Xc %*% beta))
  pi.sc <- p.sc / (1 - p.sc)
  wts.sc <- 1 / pi.sc
  
  
  # ALP mean
  if (is.null(zcol)) Z = numeric(length(wts.sc))+1 else Z = sc[,zcol]
  Y  <- sc[[y]]*Z
  T1 <- sum(Y * wts.sc)
  T2 <- sum(Z * wts.sc)
  mu <- T1 / T2
  
  # ALP variance (matrix D by bootstrap will be updated, assume Poisson sampling for now)
  U_beta <- t(Y - mu*Z) %*% (wts.sc * Xc)
  S_beta <- t(p.sc * (1 - p.sc) * Xc) %*% Xc + t(sp[,wts.col] * p.sp * (1 - p.sp) * Xp) %*% Xp
  b_vec  <- U_beta %*% solve(S_beta)
  
  D <- t(sp[,wts.col]^2 * (1 - 1 / sp[,wts.col]) * (p.sp * Xp)) %*% (p.sp * Xp)
  
  v1 <- sum(wts.sc * (wts.sc - 1) * (Y - mu*Z - p.sc * Xc %*% t(b_vec))^2)
  v2 <- b_vec %*% D %*% t(b_vec)
  v  <- as.vector((1 / T2^2) * (v1 + v2))
  
  return(list(wts_ALP = wts.sc[Z] ,beta_ALP = as.numeric(beta), 
              mean_ALP = mu, variance_ALP = v, names = colnames(Xc), iterations = iter))
}
