# Load required package
if (!requireNamespace("survey", quietly = TRUE)) {
  stop("Package 'survey' required; please install it with install.packages('survey').")
}
library(survey)

#------------------------------------------------------------------------------#
# PRECALI: two‐sample pre‐calibration function
#------------------------------------------------------------------------------#
# Args:
#   wts      : character vector of length 2, c(wt1, wt2) – names of weight columns
#   refs     : list of two data.frames, refs[[1]] is sp1, refs[[2]] is sp_i
#   dup_vars : character vector of variable names to calibrate on
# Returns:
#   Numeric vector of calibrated weights for refs[[2]]
PRECALI <- function(wts, refs, dup_vars) {
  # build survey design objects
  ds1 <- svydesign(ids = ~1,
                   weights = as.formula(paste0("~", wts[1])),
                   data    = refs[[1]])
  ds2 <- svydesign(ids = ~1,
                   weights = as.formula(paste0("~", wts[2])),
                   data    = refs[[2]])
  
  # calibration formula and population totals
  if (length(dup_vars) == 0) {
    # only total weight
    fml      <- as.formula("~1")
    pops     <- sum(weights(ds1))
    names(pops) <- "(Intercept)"
  } else {
    fml      <- as.formula(paste0("~", paste(dup_vars, collapse = " + ")))
    total    <- sum(weights(ds1))
    marg     <- coef(svytotal(fml, ds1))
    pops     <- c(`(Intercept)` = total, marg)
  }
  
  # calibrate ds2 to ds1 margins
  ds2_cal  <- calibrate(design     = ds2,
                        formula   = fml,
                        population = pops)
  # return new weights
  weights(ds2_cal)
}