process_p_formula <- function(sc, sp, weight, y, zcol, p_formula) {
  # --- One-reference case ---
  if (is.data.frame(sp)) {
    if (!inherits(p_formula, "formula")) {
      stop("'p_formula' must be a formula for one-reference case.")
    }

    vars_in_formula <- all.vars(p_formula)
    missing_sc <- setdiff(vars_in_formula, names(sc))
    missing_sp <- setdiff(vars_in_formula, names(sp))
    if (length(missing_sc) > 0 || length(missing_sp) > 0) {
      stop(paste0(
        "Missing variable(s): ",
        paste(unique(c(missing_sc, missing_sp)), collapse = ", ")
      ))
    }

    # Build design matrices
    Xc <- model.matrix(p_formula, data = sc)
    Xp <- model.matrix(p_formula, data = sp)
    if ("(Intercept)" %in% colnames(Xc)) Xc <- Xc[, -1, drop = FALSE]
    if ("(Intercept)" %in% colnames(Xp)) Xp <- Xp[, -1, drop = FALSE]

    vars <- make.names(colnames(Xc), unique = TRUE)
    colnames(Xc) <- vars; colnames(Xp) <- vars

    sc_new <- data.frame(Xc)
    sc_new[[y]] <- sc[[y]]

    if (!is.null(zcol)) for (z in zcol) sc_new[[z]] <- sc[[z]]

    sp_new <- data.frame(Xp)
    if (!(weight %in% names(sp))) stop("Weight column not found in sp.")
    sp_new[[weight]] <- sp[[weight]]

    return(list(sc = sc_new, sp = sp_new, vars = vars))
  }

  # --- Multi-reference case ---
  if (is.list(sp)) {
    if (!(is.list(p_formula) && all(vapply(p_formula, inherits, logical(1), "formula")))) {
      stop("For multi-reference, 'p_formula' must be a list of formulas.")
    }

    sp_new <- vector("list", length(sp))
    vars_list <- vector("list", length(sp))
    sc_orig <- sc

    for (j in seq_along(sp)) {
      fml <- p_formula[[j]]
      vars_in_formula <- all.vars(fml)
      missing_sc <- setdiff(vars_in_formula, names(sc))
      missing_sp <- setdiff(vars_in_formula, names(sp[[j]]))
      if (length(missing_sc) > 0 || length(missing_sp) > 0) {
        stop(paste0("For reference ", j, ", missing vars: ",
                    paste(unique(c(missing_sc, missing_sp)), collapse = ", ")))
      }

      Xc <- model.matrix(fml, data = sc)
      Xp <- model.matrix(fml, data = sp[[j]])
      if ("(Intercept)" %in% colnames(Xc)) Xc <- Xc[, -1, drop = FALSE]
      if ("(Intercept)" %in% colnames(Xp)) Xp <- Xp[, -1, drop = FALSE]
      colnames(Xc) <- make.names(colnames(Xc), unique = TRUE)
      colnames(Xp) <- make.names(colnames(Xp), unique = TRUE)

      # Merge new columns without duplicates
      if (j == 1L) sc_new <- as.data.frame(Xc) else {
        for (nm in colnames(Xc)) {
          new_col <- Xc[, nm]
          duplicate <- any(vapply(sc_new, function(old_col) all(abs(old_col - new_col) < 1e-12),
                                  logical(1)))
          if (!duplicate) sc_new[[nm]] <- new_col
        }
      }

      spj <- as.data.frame(Xp)
      wj <- weight[[j]]
      if (!(wj %in% names(sp[[j]]))) stop(paste("Weight", wj, "not found in sp[[", j, "]]"))
      spj[[wj]] <- sp[[j]][[wj]]
      sp_new[[j]] <- spj
      vars_list[[j]] <- colnames(Xc)
    }

    # Finalize
    sc <- sc_new
    if (!is.null(y)) sc[[y]] <- sc_orig[[y]]
    if (!is.null(zcol)) for (z in zcol) sc[[z]] <- sc_orig[[z]]

    return(list(sc = sc, sp = sp_new, vars = vars_list))
  }
}
