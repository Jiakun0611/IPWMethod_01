IPWM_One <- function(
    sc,                          # sc: convenience sample data.frame
    sp,                          # sp: reference sample data.frame
    y,                           # y: outcome variable in sc
    vars = NULL,                 # vars: covariates for participation model
    weight,                      # weight: weight column name in sp
    method,                      # one of "ALP","CLW","raking"
    zcol = NULL,                 # domain variable for subset
    maxit = 20, tol = 1e-4,      # NR control
    verbose = FALSE,
    log_messages) {

  validated <- check_input_one(sc=sc, sp=sp, y=y, vars=vars, wts.col = weight, zcol = zcol)
  sc   <- validated$sc
  sp   <- validated$sp
  y    <- validated$y
  vars <- validated$vars

  # --- unweighted mean ---
  result_naive <- naive_mean(df=sc, domain_var = zcol, y=y)

  # --- run chosen IPW method ---
  if (method == "ALP") {
    result_IPW <- ALP(y=y, vars=vars, sc=sc, sp=sp, wts.col=weight, zcol=zcol, maxit=maxit, tol=tol)
  } else if (method == "CLW") {
    result_IPW <- CLW(y=y, vars=vars, sc=sc, sp=sp, wts.col=weight, zcol=zcol, maxit=maxit, tol=tol)
  } else if (method == "raking") {
    result_IPW <- raking(y=y, vars=vars, sc=sc, sp=sp, wts.col=weight, zcol=zcol, maxit=maxit, tol=tol)
  }

  # --- unified output ---
  result <- list(
    mean_unweighted  = result_naive$mean,
    var_unweighted   = result_naive$var,
    CI_95_unweighted = c(
      result_naive$mean - 1.96 * sqrt(result_naive$var),
      result_naive$mean + 1.96 * sqrt(result_naive$var)
    ),
    mean_adjusted    = result_IPW[[3]],
    var_adjusted     = result_IPW[[4]],
    CI_95_adjusted   = c(
      result_IPW[[3]] - 1.96 * sqrt(result_IPW[[4]]),
      result_IPW[[3]] + 1.96 * sqrt(result_IPW[[4]])
    ),
    weights          = result_IPW[[1]],
    variables        = result_IPW[[5]],
    coefficients     = result_IPW[[2]],
    iterations       = result_IPW[[6]],
    method           = method,
    call             = match.call(),
    log_messages     = log_messages
  )



  class(result) <- "IPWM"
  return(result)
}

