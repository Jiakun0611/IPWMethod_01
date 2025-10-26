PRECALI <- function(wts, refs, dup_vars) {

  # build survey design objects
  ds1 <- survey::svydesign(ids = ~1,
                           weights = as.formula(paste0("~", wts[1])),
                           data    = refs[[1]])
  ds2 <- survey::svydesign(ids = ~1,
                           weights = as.formula(paste0("~", wts[2])),
                           data    = refs[[2]])

  # calibration formula and population totals
  if (length(dup_vars) == 0) {
    fml <- as.formula("~1")
    pops <- sum(survey::weights(ds1))
    names(pops) <- "(Intercept)"
  } else {
    fml <- as.formula(paste0("~", paste(dup_vars, collapse = " + ")))
    total <- sum(survey::weights(ds1))
    marg <- coef(survey::svytotal(fml, ds1))
    pops <- c(`(Intercept)` = total, marg)
  }

  # calibrate ds2 to ds1 margins
  ds2_cal <- survey::calibrate(
    design     = ds2,
    formula    = fml,
    population = pops
  )

  # return new weights
  survey::weights(ds2_cal)
}
