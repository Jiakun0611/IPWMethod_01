process_p_formula <- function(sc, sp, weight, y, zcol, p_formula,
                              Pre.calibration = TRUE, verbose = FALSE) {
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

    # --- Validate outcome and domain variables ---
    if (!(y %in% names(sc))) {
      stop("The outcome variable '", y, "' is not found in the convenience sample (sc).",call. = TRUE)
    }

    if (!is.null(zcol)) {
      missing_z <- setdiff(zcol, names(sc))
      if (length(missing_z) > 0) {
        stop("The following domain variable(s) are missing from the convenience sample (sc): ",
             paste(missing_z, collapse = ", "))
      }
    }

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

    # No missing variables in sc and sp data.frames
    for (j in seq_along(sp)) {
      fml <- p_formula[[j]]
      vars_in_formula <- all.vars(fml)
      missing_sc <- setdiff(vars_in_formula, names(sc))
      missing_sp <- setdiff(vars_in_formula, names(sp[[j]]))
      if (length(missing_sc) > 0 || length(missing_sp) > 0) {
        stop(paste0("For reference ", j, ", missing vars: ",
                    paste(unique(c(missing_sc, missing_sp)), collapse = ", ")))
      }

      # Build model matrices
      Xc <- model.matrix(fml, data = sc)
      Xp <- model.matrix(fml, data = sp[[j]])
      if ("(Intercept)" %in% colnames(Xc)) Xc <- Xc[, -1, drop = FALSE]
      if ("(Intercept)" %in% colnames(Xp)) Xp <- Xp[, -1, drop = FALSE]
      colnames(Xc) <- make.names(colnames(Xc), unique = TRUE)
      colnames(Xp) <- make.names(colnames(Xp), unique = TRUE)

      # Merge covariates into sc without duplicates
      if (j == 1L) {
        sc_new <- as.data.frame(Xc)
      } else {
        for (nm in colnames(Xc)) {
          new_col <- Xc[, nm]
          duplicate <- any(vapply(sc_new, function(old_col) {
            if (is.numeric(old_col) && is.numeric(new_col)) {
              isTRUE(all.equal(old_col, new_col, tolerance = 1e-12))
            } else {
              identical(old_col, new_col)
            }
          }, logical(1)))
          if (!duplicate) sc_new[[nm]] <- new_col
        }
      }


      spj <- as.data.frame(Xp)
      wj <- weight[[j]]
      if (!(wj %in% names(sp[[j]])))
        stop(paste("Weight", wj, "not found in sp[[", j, "]]"))
      spj[[wj]] <- sp[[j]][[wj]]
      sp_new[[j]] <- spj
      vars_list[[j]] <- colnames(Xc)
    }

    # --- preserve original names ---
    names(sp_new) <- if (!is.null(names(sp))) names(sp) else paste0("sp", seq_along(sp))

    # --- restore outcome and domain vars ---
    sc <- sc_new
    if (!is.null(y)) sc[[y]] <- sc_orig[[y]]
    if (!is.null(zcol)) for (z in zcol) sc[[z]] <- sc_orig[[z]]


    # ---- optional pairwise pre-calibration (use largest sp as reference; raw sp fully expanded) ----
    log_messages <- character()

    if (Pre.calibration && length(sp_new) > 1) {
      sizes <- sapply(sp, nrow)
      ref_idx <- which.max(sizes)
      ref_name <- names(sp)[ref_idx]
      if (is.null(ref_name)) ref_name <- paste0("sp", ref_idx)

      msg <- sprintf("\nPre-calibration summary:\nPre-calibration reference: %s (n = %d)\n",
                     ref_name, sizes[ref_idx])
      log_messages <- c(log_messages, msg)
      if (verbose) cat(msg)

      for (i in seq_along(sp)) {
        if (i == ref_idx) next

        # --- find shared variables (exclude weight columns) ---
        ref_vars <- setdiff(names(sp[[ref_idx]]), weight[[ref_idx]])
        tar_vars <- setdiff(names(sp[[i]]),        weight[[i]])
        shared   <- intersect(ref_vars, tar_vars)

        # --- use shared variables directly (no fallback dummy) ---
        ref_use <- if (length(shared) > 0) sp[[ref_idx]][, shared, drop = FALSE] else sp[[ref_idx]][, 0, drop = FALSE]
        tar_use <- if (length(shared) > 0) sp[[i]][, shared, drop = FALSE] else sp[[i]][, 0, drop = FALSE]

        # --- temporary frames for PRECALI ---
        ref_tmp <- cbind(ref_use, w = sp[[ref_idx]][[weight[[ref_idx]]]])
        tar_tmp <- cbind(tar_use, w = sp[[i]][[weight[[i]]]])

        # --- run PRECALI ---
        new_w <- PRECALI(
          wts      = c("w", "w"),
          refs     = list(ref_tmp, tar_tmp),
          dup_vars = shared
        )

        # --- update target weights ---
        sp_new[[i]][[ weight[[i]] ]] <- new_w

        # --- logging ---
        if (length(shared) > 0) {
          msg <- sprintf(
            "Pre-calibration for %s done on: %s and survey weights total\n",
            names(sp)[i] %||% paste0("sp", i),
            paste(shared, collapse = ", ")
          )
        } else {
          msg <- sprintf(
            "Pre-calibration for weights in %s done on survey weights total (no shared variables)\n",
            names(sp)[i] %||% paste0("sp", i)
          )
        }

        log_messages <- c(log_messages, msg)
        if (verbose) cat(msg)
      }

    } else {
      msg <- "Pre-calibration is recommended.\n"
      log_messages <- c(log_messages, msg)
      if (verbose) cat(msg)
    }


    # ======================================================================

    return(list(sc = sc, sp = sp_new, vars = vars_list, log_messages = log_messages))
  }
}
