# Complete IPWM_Multi consist of: 1) reorder
#                                 2) input check
#                                 3) pairwise pre‐calibration
#                                 4) design matrix construction
#                                 5) Newton–Raphson algorithm
#                                 6) Matrix D construction

IPWM_Multi_Raking <- function(
    sc, sp, y, vars = list(), weight,
    cali = TRUE, zcol = NULL,
    maxit = 20, tol = 1e-4, verbose = FALSE) {

  result_naive <- naive_mean(df=sc, domain_var = zcol, y=y)

  # initialize log
  log_messages <- character()

  # --- 1) Reorder by size ---
  sorted        <- sort_by_sp_size(sp, vars, weight, verbose = verbose)
  sp_sorted     <- sorted$sp
  vars_sorted   <- sorted$vars
  weight_sorted <- sorted$weight
  log_messages  <- c(log_messages, sorted$log)

  # --- 2) Validate inputs & pre-calibration ---
  valid <- check_input_multi(
    sc              = sc,
    sp_list         = sp_sorted,
    vars_list       = vars_sorted,
    wts_cols        = weight_sorted,
    Pre.calibration = cali,
    verbose         = verbose
  )
  sc       <- valid$sc
  sp_list  <- valid$sp_list
  vars_XC  <- valid$vars_XC
  xcol     <- valid$xcol
  wts_cols <- valid$wts_cols
  log_messages <- c(log_messages, valid$log)

  # --- 3) Design matrices ---
  processed <- Xc_Xp_Construction(vars_XC, sc, sp_list, xcol, wts_cols)
  Xc       <- processed$Xc
  Xp_list  <- processed$Xp_list
  wts_list <- processed$wts_list


  # --- 4) Newton–Raphson for Raking ---
  nr_out <- raking_nr(Xc, Xp_list, wts_list, maxit = maxit, tol = tol)
  beta   <- nr_out$beta
  wts.sc <- 1 / exp(as.vector(Xc %*% beta))


  # --- 5) Mean and variance estimation ---
  Z <- if (is.null(zcol)) rep(1, length(wts.sc)) else sc[, zcol]
  Y <- sc[[y]] * Z

  T1 <- sum(Y * wts.sc)
  T2 <- sum(Z * wts.sc)
  mu <- T1 / T2

  D <- make_block_D(sp_list, wts_cols, xcol)
  U_beta <- t(Y - mu * Z) %*% (wts.sc * Xc)
  S_beta <- t(wts.sc * Xc) %*% Xc
  b_vec  <- U_beta %*% solve(S_beta)

  resid <- Y - mu * Z - Xc %*% t(b_vec)
  v1 <- sum(wts.sc * (wts.sc - 1) * resid^2)
  v2 <- as.numeric(b_vec %*% D %*% t(b_vec))
  v  <- (v1 + v2) / (T2^2)

  names(beta) <- c("(Intercept)", vars_XC)

  # --- unified output ---
  result <- list(
    mean_unweighted  = result_naive$mean,
    var_unweighted   = result_naive$var,
    CI_95_unweighted = c(
      result_naive$mean - 1.96 * sqrt(result_naive$var),
      result_naive$mean + 1.96 * sqrt(result_naive$var)
    ),
    mean_adjusted    = mu,
    var_adjusted     = v,
    CI_95_adjusted   = c(
      mu - 1.96 * sqrt(v),
      mu + 1.96 * sqrt(v)
    ),
    weights          = wts.sc[Z == 1],
    variables        = names(beta),
    coefficients     = beta,
    iterations       = nr_out$iter,
    method           = "multi",
    call             = match.call(),
    log              = log_messages
  )


  class(result) <- "IPWM"
  return(result)
}
