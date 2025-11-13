check_input_one <- function(sc, sp, y, vars, wts.col, zcol) {

  # --- standardize names ---
  colnames(sc) <- make.names(colnames(sc))
  colnames(sp) <- make.names(colnames(sp))
  y <- make.names(y)

  # --- ensure vars is character vector ---
  if (is.list(vars)) vars <- unlist(vars)
  if (!is.character(vars)) vars <- as.character(vars)

  # --- handle missing values ---
  miss_sc <- colSums(is.na(as.data.frame(sc[vars])))
  miss_sp <- colSums(is.na(as.data.frame(sp[vars])))
  cols_with_na <- unique(c(names(miss_sc)[miss_sc > 0],
                           names(miss_sp)[miss_sp > 0]))

  if (length(cols_with_na) > 0) {
    message("Missing values detected in columns: ",
            paste(cols_with_na, collapse = ", "))
    sc <- sc[complete.cases(sc[vars]), ]
    sp <- sp[complete.cases(sp[vars]), ]
    message("Performed complete-case filtering: ",
            nrow(sc), " rows in sc; ",
            nrow(sp), " rows in sp.")
  }

  # --- ensure factor levels consistent ---
  for (var in vars) {
    is_fac_sc <- is.factor(sc[[var]])
    is_fac_sp <- is.factor(sp[[var]])
    if (is_fac_sc || is_fac_sp) {
      if (!(is_fac_sc && is_fac_sp)) {
        stop("Variable '", var, "' is factor in one dataset but not the other.")
      }
      if (!identical(levels(sc[[var]]), levels(sp[[var]]))) {
        stop("Factor levels for '", var, "' differ between sc and sp:\n  sc: ",
             paste(levels(sc[[var]]), collapse = ", "),
             "\n  sp: ",
             paste(levels(sp[[var]]), collapse = ", "))
      }
    }
  }

  return(list(sc = sc, sp = sp, y = y, vars = vars))
}
