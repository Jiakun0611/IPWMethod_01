#' Summary method for IPWM objects
#'
#' Provides detailed output for objects of class \code{"IPWM"},
#' including model information, estimators, and selection model coefficients.
#'
#' @param object An object of class \code{"IPWM"}, typically the output of \code{\link{IPWM}}.
#' @param ... Additional arguments (not used).
#'
#' @method summary IPWM
#' @export
#'
summary.IPWM <- function(object, ...) {

  # --- One-reference case ---
  if (object$method %in% c("ALP", "CLW", "raking")) {
    cat("Call:\n")
    print(object$call)
    cat("\nMethod: One Reference", object$method, "\n\n")
    cat("Inputs are valid\n\n",
        "participation model involves the following variables:\n", object$variables[-1], '\n')

    cat("\nEstimators:\n\n")
    cat("Unweighted mean:    ", object$mean_naive, '\n')
    cat("Unweighted variance:", object$var_naive, '\n\n')

    cat(object$method, "mean:    ", object$mean, "\n")
    cat(object$method, "variance:", object$variance, "\n")
    cat("(Newton-Raphson iterations:", object$iterations, ")\n\n")

    cat("Selection model coefficients:\n")

    names  <- object$variables
    values <- object$coefficients

    df <- rbind(values)
    colnames(df) <- names
    rownames(df) <- NULL

    formatted_df <- format(round(df, 4), justify = "left", width = 10)
    print(as.data.frame(formatted_df), row.names = FALSE)
  }

  # --- Multi-reference case ---
  else if (object$method == "multi") {
    cat("Call:\n")
    print(object$call)
    cat("\nMethod: Multi Reference\n\n")

    if (!is.null(object$log) && length(object$log) > 0) {
      cat(paste0(object$log, collapse = ""))
      cat("\n")
    }

    cat("Estimators:\n\n")
    cat("Unweighted mean:    ", object$mean_naive, "\n")
    cat("Unweighted variance:", object$var_naive, "\n\n")

    cat(object$method, "mean:    ", object$mean, "\n")
    cat(object$method, "variance:", object$variance, "\n")
    cat("(Newton-Raphson iterations:", object$iterations, ")\n\n")

    if (!is.null(object$coefficients)) {
      cat("Selection model coefficients:\n\n")
      names  <- object$variables
      values <- object$coefficients

      df <- rbind(values)
      colnames(df) <- names
      rownames(df) <- NULL

      formatted_df <- format(round(df, 4), justify = "left", width = 10)
      print(as.data.frame(formatted_df), row.names = FALSE)
    }
  }

  invisible(object)
}
