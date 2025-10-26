check_input <- function(sc, sp, y, vars, wts.col, method, zcol) {

  # method verification
  valid_methods <- c("ALP", "CLW", "raking")
  if (!(method %in% valid_methods)) {
    stop("Method must be one of: ", paste(valid_methods, collapse = ", "),
         ". Provided method: '", method, "'")
  }

  if (is.null(vars)) {
    vars <- intersect(colnames(sc), colnames(sp))
  }
  if (is.null(vars)) {
    stop("No shared variables exist between two samples")
  }

  # check missing values in sc[,vars] and sp[,vars]

  # ensure vars is character vector
  if (is.list(vars)) vars <- unlist(vars)
  if (!is.character(vars)) vars <- as.character(vars)

  miss_sc <- colSums(is.na(as.data.frame(sc[vars])))
  miss_sp <- colSums(is.na(as.data.frame(sp[vars])))

  cols_with_na <- unique(c(names(miss_sc)[miss_sc > 0],
                           names(miss_sp)[miss_sp > 0]))
  if (length(cols_with_na) > 0) {
    cat("Missing values detected in columns:\n")
    if (any(miss_sc > 0)) {
      cat("  sc:", paste(names(miss_sc)[miss_sc > 0], collapse = ", "), "\n")
    }
    if (any(miss_sp > 0)) {
      cat("  sp:", paste(names(miss_sp)[miss_sp > 0], collapse = ", "), "\n")
    }
    # complete‐case analysis on those vars
    sc <- sc[complete.cases(sc[vars]), ]
    sp <- sp[complete.cases(sp[vars]), ]
    cat("Performed complete‐case analysis: ",
        nrow(sc), " rows in sc; ",
        nrow(sp), " rows in sp.\n")
  }

  # check that any factor vars have identical levels in sc and sp
  for (var in vars) {
    is_fac_sc <- is.factor(sc[[var]])
    is_fac_sp <- is.factor(sp[[var]])
    if (is_fac_sc || is_fac_sp) {
      if (! (is_fac_sc && is_fac_sp)) {
        stop("Variable '", var, "' is factor in one dataset but not the other.")
      }
      if (! identical(levels(sc[[var]]), levels(sp[[var]]))) {
        stop("Factor levels for '", var,
             "' differ between sc and sp:\n  sc: ",
             paste(levels(sc[[var]]), collapse = ", "),
             "\n  sp: ",
             paste(levels(sp[[var]]), collapse = ", "))
      }
    }
  }

  colnames(sc) <- make.names(colnames(sc))
  colnames(sp) <- make.names(colnames(sp))
  y <- make.names(y)

  # Outcome variable must be present in the convenience sample (`sc`).
  if (!(y %in% colnames(sc))) {
    stop("The outcome is not in the convenience sample (sc). Please check the name of 'y'.")
  }

  # Domain variable must be present in the convenience sample (`sc`) if specified.
  if (!is.null(zcol) && !(zcol %in% colnames(sc))) {
    stop("The domain variable is not in the convenience sample (sc). Please check the name of 'zcol'.")
  }

  # Weights column must be present in the probability sample (`sp`).
  if (!(wts.col %in% colnames(sp))) {
    stop(paste0("The probability sample (sp) does not contain the required weights column: '", wts.col, "'."))
  }

  missing.vars <- setdiff(vars, intersect(colnames(sc), colnames(sp)))
  if (length(missing.vars) > 0) {
    stop(paste("The following variable(s) are missing from one or both datasets:",
               paste(missing.vars, collapse = ", ")))
  }

  # unused.vars <- setdiff(intersect(colnames(sc), colnames(sp)), vars)
  # if (length(unused.vars) > 0) {
  #   warning(paste("The following shared variables are not included in 'vars':\n",
  #                 paste(unused.vars, collapse = ", ")))
  # }

  return(list(sc = sc, sp = sp, y = y, vars = vars))
}

