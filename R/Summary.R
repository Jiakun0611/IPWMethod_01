#' Summary method for IPWM objects
#'
#' Provides detailed output for objects of class \code{"IPWM"},
#' including model information, estimators, confidence intervals,
#' and selection model coefficients.
#'
#' @param object An object of class \code{"IPWM"}, typically the output of \code{\link{IPWM}}.
#' @param ... Additional arguments (not used).
#'
#' @method summary IPWM
#' @export
#'
summary.IPWM <- function(object, ...) {

  cat("Call:\n")
  print(object$call)
  cat("\nMethod:", object$method, "\n\n")

  # --- Optional log output (for multi-reference runs) ---
  if (!is.null(object$log) && length(object$log) > 0) {
    cat(paste0(object$log, collapse = ""))
    cat("\n")
  }

  # --- Model information ---
  if (!is.null(object$variables)) {
    cat("Participation model involves the following variables:\n")
    cat(object$variables[-1], "\n\n")
  }

  # --- Estimators section ---
  cat("Estimators:\n\n")
  cat("Unweighted mean:     ", round(object$mean_unweighted, 6), "\n")
  cat("Unweighted variance: ", round(object$var_unweighted, 6), "\n")
  cat("95% CI (unweighted): [",
      round(object$CI_95_unweighted[1], 6), ", ",
      round(object$CI_95_unweighted[2], 6), "]\n\n")

  cat("Adjusted mean:       ", round(object$mean_adjusted, 6), "\n")
  cat("Adjusted variance:   ", round(object$var_adjusted, 6), "\n")
  cat("95% CI (adjusted):   [",
      round(object$CI_95_adjusted[1], 6), ", ",
      round(object$CI_95_adjusted[2], 6), "]\n")
  cat("(Newton-Raphson iterations:", object$iterations, ")\n\n")

  # --- Coefficients section ---
  if (!is.null(object$coefficients)) {
    cat("Selection model coefficients:\n\n")

    df <- rbind(object$coefficients)
    colnames(df) <- object$variables
    rownames(df) <- NULL

    formatted_df <- format(round(df, 4), justify = "left", width = 10)
    print(as.data.frame(formatted_df), row.names = FALSE)
  }

  invisible(object)
}
