#' Print method for IPWM objects
#'
#' This function defines how objects of class \code{"IPWM"} are printed
#' in the console. It displays the call, the estimation method, and
#' key summary statistics (mean and variance).
#'
#' @param obj An object of class \code{"IPWM"}, typically returned by \code{\link{IPWM}}.
#' @param ... Additional arguments (not used).
#'
#' @method print IPWM
#' @export
#'
print.IPWM <- function(obj, ...) {
  cat("Call:\n")
  print(obj$call)

  cat("\nEstimators:\n")
  cat(obj$method, "mean:     ", round(obj$mean_adjusted, 6), "\n")
  cat(obj$method, "variance: ", round(obj$var_adjusted, 6), "\n")
  cat(obj$method, "95% CI:   [",
      round(obj$CI_95_adjusted[1], 6), ", ",
      round(obj$CI_95_adjusted[2], 6), "]\n")

  invisible(obj)
}



