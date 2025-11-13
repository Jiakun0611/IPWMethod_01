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
#'
#'   \item{mean_unweighted}{Unweighted (raw) mean of the outcome variable.}
#'   \item{var_unweighted}{Unweighted variance of the outcome variable.}
#'   \item{CI_95_unweighted}{95\% confidence interval for the unweighted mean.}
#'
#'   \item{mean_adjusted}{Weighted mean estimated using the specified IPW method.}
#'   \item{var_adjusted}{Variance estimate for the weighted mean.}
#'   \item{CI_95_adjusted}{95\% confidence interval for the weighted mean.}
#'
#'   \item{weights}{Final calibrated or IPW-adjusted weights assigned to each unit in the convenience sample.}
#'   \item{variables}{Covariates included in the participation model.}
#'   \item{coefficients}{Estimated coefficients from the participation model (logistic regression).}
#'   \item{iterations}{Number of iterations used in the estimation procedure.}
#' }

#'
#' The object can be summarized with \code{summary()} to display key results.
#'
#'
#' @details
#' For a detailed overview of R formula syntax, open the PDF included in the package:
#'
#' \preformatted{
#' browseURL(system.file("Doc", "formula_cheatsheet.pdf", package = "IPWMethod"))
#' }
#'
#' Simply copy and run this line in R to view the cheatsheet.

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
#' @export


IPWM <- function(
    sc,                          # sc: convenience sample data.frame
    sp,                          # sp: data.frame (One) or list of data.frames (Multi)
    y,                           # y: outcome variable in sc
    weight,                      # weight: string (One) or vector (Multi)
    p_formula = NULL,            # p_formula: formula (One) or list of formulas (Multi)
    method = NULL,               # method: "ALP", "CLW", "raking", or "multi"
    zcol = NULL,                 # zcol: domain variable for subset calculation
    cali = TRUE,                 # cali: whether to run pairwise pre-calibration (Multi only)
    maxit = 20,                  # maxit: maximum Newton–Raphson iterations
    tol = 1e-4,                  # tol: convergence tolerance for Newton–Raphson
    verbose = FALSE              # verbose: print intermediate updates
) {

  #--------------------------------------------------------------------------#
  # 0. Basic input validation
  #--------------------------------------------------------------------------#
  check_ipwm_inputs(sc, sp, y, weight, p_formula, method, zcol)

  #--------------------------------------------------------------------------#
  # 1. Detect single vs. multiple reference cases
  #--------------------------------------------------------------------------#
  if (is.list(sp) && all(vapply(sp, is.data.frame, logical(1)))) {
    # Multi-reference case
    if (is.null(method)) {
      method <- "multi"
    } else if (method != "multi") {
      warning("For list-type 'sp', method forcibly set to 'multi'.", call. = FALSE)
      method <- "multi"
    }
  } else if (is.data.frame(sp)) {
    # One-reference case
    if (is.null(method)) {
      stop("Please specify method = 'ALP', 'CLW', or 'raking' for one-reference case.",
           call. = FALSE)
    }
  } else {
    stop("'sp' must be a data.frame (one reference) or a list of data.frames (multi reference).",
         call. = FALSE)
  }

  #--------------------------------------------------------------------------#
  # 2. Build (if omitted by user) and preprocess p_formula
  #--------------------------------------------------------------------------#
  log_messages <- character(0)

  # Auto-build formula if user did not supply one
  if (is.null(p_formula)) {
    built <- p_formula_construction(sc, sp, y, weight, zcol, verbose = verbose)
    p_formula <- built$p_formula
    log_messages <- c(log_messages, built$log_messages)
  }

  # Process inputs and optionally run pre-calibration
  processed <- tryCatch(
    process_p_formula(sc, sp, weight, y, zcol, p_formula, cali, verbose),
    error = function(e)
      stop("Input processing failed: ", e$message, call. = TRUE)
  )

  sc     <- processed$sc
  sp     <- processed$sp
  p_vars <- processed$vars
  if (!is.null(processed$log_messages)) {
    log_messages <- c(log_messages, processed$log_messages)
  }

  #--------------------------------------------------------------------------#
  # 3. Estimation
  #--------------------------------------------------------------------------#

  # --- One-reference ALP/CLW/Raking ---
  if (method %in% c("ALP", "CLW", "raking")) {
    result <- tryCatch(
      IPWM_One(
        sc      = sc,
        sp      = sp,
        y       = y,
        vars    = p_vars,
        weight  = weight,
        method  = method,
        zcol    = zcol,
        maxit   = maxit,
        tol     = tol,
        verbose = verbose,
        log_messages = log_messages
      ),
      error = function(e)
        stop("Estimation failed in one-reference method: ", e$message, call. = TRUE)
    )
  }

  # --- Multi-reference Raking ---
  else if (method == "multi") {
    result <- tryCatch(
      IPWM_Multi_Raking(
        sc        = sc,
        sp        = sp,
        y         = y,
        vars      = p_vars,
        weight    = weight,
        cali      = cali,
        zcol      = zcol,
        maxit     = maxit,
        tol       = tol,
        verbose   = verbose,
        log_messages    = log_messages
      ),
      error = function(e)
        stop("Estimation failed in multi-reference raking: ", e$message, call. = TRUE)
    )
    result$method <- "multi"
  }

  #--------------------------------------------------------------------------#
  # 4. Finalize and return
  #--------------------------------------------------------------------------#
  class(result) <- "IPWM"
  result$call <- match.call()
  return(result)
}

