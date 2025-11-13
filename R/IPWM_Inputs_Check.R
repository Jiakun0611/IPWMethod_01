#-------------------------------------------------------------
# Basic structural validation for IPWM inputs
#-------------------------------------------------------------
check_ipwm_inputs <- function(sc, sp, y, weight, p_formula, method, zcol) {
  # --- sc ---
  if (!is.data.frame(sc)) {
    stop("'sc' must be a data.frame.", call. = FALSE)
  }

  # --- sp and weight column---
  if (is.data.frame(sp)) {
    if (!is.character(weight) || length(weight) != 1L) {
      stop("For single-reference case, 'weight' must be a single string.", call. = FALSE)
    }
  } else if (is.list(sp) && all(vapply(sp, is.data.frame, logical(1)))) {
    if (!is.character(weight) || length(weight) != length(sp)) {
      stop("For multi-reference case, 'weight' must be a character vector with one entry per reference sample.",
           call. = FALSE)
    }
  } else {
    stop("'sp' must be either a data.frame or a list of data.frames.", call. = FALSE)
  }

  # --- y ---
  if (!is.character(y) || length(y) != 1L) {
    stop("'y' must be a single character string naming the outcome variable.", call. = FALSE)
  }
  if (!(y %in% names(sc))) {
    stop(sprintf("Outcome variable '%s' not found in 'sc'.", y), call. = FALSE)
  }

  # --- method ---
  valid_methods <- c("ALP", "CLW", "raking", "multi", NULL)
  if (!is.null(method) && !(method %in% valid_methods)) {
    stop(sprintf("Invalid method '%s'. Must be one of: %s.",
                 method, paste(valid_methods[!is.null(valid_methods)], collapse = ", ")),
         call. = FALSE)
  }

  # --- zcol ---
  if (!is.null(zcol) && !(zcol %in% names(sc))) {
    stop(sprintf("Domain variable '%s' not found in 'sc'.", zcol), call. = FALSE)
  }

  invisible(TRUE)
}
