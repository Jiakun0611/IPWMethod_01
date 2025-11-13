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

  if (object$method %in% c("ALP", "CLW", "raking")) {
    cat("\nMethod: One reference", object$method, "\n")
  } else if (object$method == "multi") {
    cat("\nMethod: Multi reference raking\n")
  }

  # --- Optional log output (for multi-reference runs) ---
  if (!is.null(object$log_messages) && length(object$log_messages) > 0) {
    cat(paste0(object$log_messages, collapse = ""))
  }

  # --- Model information ---
  if (!is.null(object$variables)) {
    cat("\nParticipation model involves the following variables:\n")
    cat(object$variables[-1], "\n\n")
  }

  # --- Estimators section ---
  cat("Estimators summary:\n")

  cat(sprintf("  %-22s %10.6f\n", "Unweighted mean:",     object$mean_unweighted))
  cat(sprintf("  %-22s %10.6f\n", "Unweighted variance:", object$var_unweighted))
  cat(sprintf("  %-22s [%10.6f, %10.6f]\n\n",
              "Unweighted 95% CI:",
              object$CI_95_unweighted[1],
              object$CI_95_unweighted[2]))

  cat(sprintf("  %-22s %10.6f\n", "Adjusted mean:",     object$mean_adjusted))
  cat(sprintf("  %-22s %10.6f\n", "Adjusted variance:", object$var_adjusted))
  cat(sprintf("  %-22s [%10.6f, %10.6f]\n",
              "Adjusted 95% CI:",
              object$CI_95_adjusted[1],
              object$CI_95_adjusted[2]))

  cat(sprintf("  (Newton–Raphson iterations: %d)\n\n", object$iterations))


  # --- Coefficients section ---
  if (!is.null(object$coefficients)) {
    cat("Selection model coefficients:\n")

    df <- rbind(object$coefficients)
    colnames(df) <- object$variables
    rownames(df) <- NULL

    formatted_df <- format(round(df, 4), justify = "left", width = 10)
    print(as.data.frame(formatted_df), row.names = FALSE)
  }

  invisible(object)
}
