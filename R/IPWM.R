#' @title Inverse Propensity Weighting Method (IPWM)
#'
#' @description
#' A unified function for implementing inverse propensity weighting methods
#' to integrate convenience samples with one or multiple reference probability
#' samples. Supports three single-reference methods - Adjusted Logistic
#' Propensity (ALP), CLW, and Raking - as well as a multi-reference extension
#' that performs calibration (raking) with optional pairwise pre-calibration.
#'
#' @details
#' The function automatically detects whether the user provides one or multiple
#' reference samples. When \code{sp} is a list of data frames, the method is
#' internally set to \code{"multi"}.
#'
#' The participation model can be specified via:
#' \itemize{
#'   \item A formula (e.g., \code{~ x1 + x2 + x3*x4}) for the one-reference case.
#'   \item A list of formulas for the multi-reference case.
#' }
#'
#' @param sc Data frame. Convenience sample containing outcome and covariates.
#' @param sp Data frame (one-reference) or list of data frames (multi-reference)
#'   representing reference probability sample(s).
#' @param y Character string. Name of the outcome variable in \code{sc}.
#' @param weight Character string (one-reference) or character vector (multi-reference)
#'   specifying the weight variable(s) in \code{sp}.
#' @param p_formula Optional. Either a formula, or a list of formulas. If omitted,
#'   the method will use main effects of existing variables directly.
#' @param method Character string. One of \code{"ALP"}, \code{"CLW"}, \code{"raking"},
#'   or \code{"multi"}. If \code{sp} is a list, method is automatically set to \code{"multi"}.
#' @param zcol Optional character vector. Domain variable(s) for subset estimation
#'   or stratified calibration.
#' @param cali Logical. Whether to perform pairwise pre-calibration in the
#'   multi-reference case. Default is \code{TRUE}.
#' @param maxit Integer. Maximum number of Newton–Raphson iterations (default 20).
#' @param tol Numeric. Convergence tolerance for the Newton–Raphson algorithm (default 1e-4).
#' @param verbose Logical. To control output format for S3 object.
#'
#' @return
#' An object of class \code{"IPWM"} containing the following elements:
#' \describe{
#'   \item{call}{Function call used to generate the result.}
#'   \item{method}{The IPW method applied ("ALP", "CLW", "raking", or "multi").}
#'   \item{mean, variance}{Estimated weighted mean and variance of the outcome.}
#'   \item{coefficients}{Estimated coefficients of the participation model.}
#'   \item{iterations}{Number of iterations used in estimation.}
#'   \item{variables}{Covariates included in the participation model.}
#' }
#'
#' The object can be summarized with \code{summary()} to display key results.
#'
#' @examples
#' \dontrun{
#' # --- Example: One-reference case ---
#' set.seed(1)
#' sc <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' sp <- data.frame(x1 = rnorm(200), x2 = rnorm(200), wt = runif(200, 0.5, 2))
#'
#' fit1 <- IPWM(
#'   sc = sc,
#'   sp = sp,
#'   y = "y",
#'   weight = "wt",
#'   p_formula = ~ x1 + x2,
#'   method = "ALP"
#' )
#'
#' # --- Example: Multi-reference case ---
#' sp1 <- data.frame(x1 = rnorm(200), x2 = rnorm(200), wt_sp1 = runif(200))
#' sp2 <- data.frame(x3 = rnorm(300), x4 = rnorm(300), wt_sp2 = runif(300))
#'
#' fit2 <- IPWM(
#'   sc = sc,
#'   sp = list(sp1, sp2),
#'   y = "y",
#'   weight = c("wt_sp1", "wt_sp2"),
#'   p_formula = list(~ x1 + x2, ~ x3 * x4),
#'   cali = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link{IPWM_One}}, \code{\link{IPWM_Multi_Raking}}, \code{\link{process_p_formula}}
#'
#' @export

IPWM <- function(
    sc,                          # sc: convenience sample data.frame
    sp,                          # sp: data.frame (One) or list of data.frames (Multi)
    y,                           # y: outcome variable in sc
    weight,                      # weight: string (One) or vector (Multi)
    p_formula = NULL,            # p_formula: formula (One) or list of formulas (Multi)
    method = NULL,               # method: one of "ALP", "CLW", "raking" (auto "multi" for list-type sp)
    zcol = NULL,                 # zcol: domain variable for subset calculation
    cali = TRUE,                 # cali: whether to run pairwise pre-calibration (Multi only)
    maxit = 20,                  # maxit: maximum Newton–Raphson iterations
    tol = 1e-4,                  # tol: convergence tolerance for Newton–Raphson
    verbose = FALSE              # verbose: print intermediate updates
) {

  # --- 0) Detect single vs. multiple reference samples ---
  if (is.list(sp) && !is.data.frame(sp) && all(vapply(sp, is.data.frame, logical(1)))) {
    # Multi-reference case
    if (is.null(method)) method <- "multi"
    if (method != "multi") {
      warning("For list-type 'sp', method forcibly set to 'multi'.")
      method <- "multi"
    }
  } else if (is.data.frame(sp)) {
    # One-reference case
    if (is.null(method)) {
      stop("Please specify method = 'ALP', 'CLW', or 'raking' for one-reference case.")
    }
  } else {
    stop("Argument 'sp' must be either a data.frame (one reference) or a list of data.frames (multi reference).")
  }


  # --- 0.5) Check and preprocess p_formula ---
  p_vars <- NULL

  if (!is.null(p_formula)) {
    # If p_formula is a formula (or list of formulas) → preprocess
    if (inherits(p_formula, "formula") ||
        (is.list(p_formula) && all(vapply(p_formula, inherits, logical(1), "formula")))) {

      processed <- process_p_formula(sc, sp, weight, y, zcol, p_formula)
      sc     <- processed$sc
      sp     <- processed$sp
      p_vars <- processed$vars   # expanded variable names

    } else {
      # Otherwise, if p_formula is already variable names, skip processing
      p_vars <- p_formula
    }
  }


  # --- 1) Run one-reference IPW method ---
  if (method %in% c("ALP", "CLW", "raking")) {
    result <- IPWM_One(
      sc      = sc,
      sp      = sp,
      y       = y,
      vars    = p_vars,
      weight  = weight,
      method  = method,
      zcol    = zcol,
      maxit   = maxit,
      tol     = tol
    )
  }

  # --- 2) Run multi-reference Raking method ---
  else if (method == "multi") {
    result <- IPWM_Multi_Raking(
      sc       = sc,
      sp       = sp,
      y        = y,
      vars     = p_vars,
      weight   = weight,
      cali     = cali,
      zcol     = zcol,
      maxit    = maxit,
      tol      = tol,
      verbose  = verbose
    )
    result$method <- "multi"
  }

  # --- 3) Invalid method ---
  else {
    stop("Unsupported method. Must be one of 'ALP', 'CLW', 'raking', or 'multi'.")
  }

  # --- 4) Return unified result object ---
  class(result) <- "IPWM"
  result$call <- match.call()
  return(result)
}
