
CLW <- function(y, vars, sc, sp, wts.col, zcol, maxit=maxit, tol=tol) {
  # Design matrices
  Xc <-  design_matrix(vars = vars, data = sc)
  Xp <-  design_matrix(vars = vars, data = sp)
  
  beta <- rep(0, ncol(Xc))
  
  # Scale beta[1] to prevent singular matrix in first iteration
  pi.sp <- expit(as.vector(Xp %*% beta))
  f.p <- colSums(sp[,wts.col] * pi.sp * Xp)
  f.c <- colSums(Xc)
  beta[1] <- -log(f.p[1] / f.c[1])
  
  # Newton-Raphson
  iter <- 0
  crit <- TRUE
  
  while (crit) {
    
    pi.sp <- expit(as.vector(Xp %*% beta))
    
    f.p <- colSums(sp[,wts.col] * pi.sp * Xp)
    f.c <- colSums(Xc)
    f <- f.c - f.p
    
    fprime <- - t(sp[,wts.col] * pi.sp * (1-pi.sp) * Xp) %*% Xp
    
    delta <- solve(fprime, f)
    beta <- beta - delta
    
    iter <- iter + 1
    crit <- iter < maxit && sum(abs(delta)) > tol
  }
  
  # Final weights
  pi.sp <- as.vector(expit(Xp %*% beta))
  pi.sc <- as.vector(expit(Xc %*% beta))
  wts.sc <- 1/pi.sc
  wts.sp <- 1/pi.sp
  
  # CLW mean with domain variable support
  if (is.null(zcol)) Z = numeric(length(wts.sc))+1 else Z = sc[,zcol]
  Y  <- sc[[y]]*Z
  T1 <- sum(Y * wts.sc)
  T2 <- sum(Z * wts.sc)
  mu <- T1 / T2
  
  # CLW variance (matrix D by bootstrap will be updated, assume Poisson sampling for now)
  U_beta <- t(Y-mu*Z) %*% ((wts.sc-1) * Xc) 
  S_beta <- t(sp[,wts.col] * pi.sp * (1-pi.sp) * Xp) %*% Xp   
  b_vec  <- U_beta %*%solve(S_beta)
  
  D <- t(sp[,wts.col]^2 * (1-1/sp[,wts.col]) *(pi.sp* Xp)) %*% (pi.sp*Xp) 
  
  v1 <- sum(wts.sc * (wts.sc-1) * (Y - mu*Z - pi.sc * Xc %*% t(b_vec))^2)  
  v2 <- (b_vec %*% D) %*% t(b_vec)  
  v  <- as.vector((1/T2^2) * (v1+v2)) 
  
  return(list(wts_CLW = wts.sc[Z],beta_CLW = as.numeric(beta), 
              mean_CLW = mu, variance_CLW = v, names = colnames(Xc), iterations = iter))
}
