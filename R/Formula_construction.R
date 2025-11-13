# -------------------------------------------------------------------
# Construct participation model formula(s) automatically
# -------------------------------------------------------------------
p_formula_construction <- function(sc, sp, y, weight, zcol = NULL, verbose = TRUE) {

  log_messages <- character(0)

  # --- One-reference case ---
  if (is.data.frame(sp)) {
    shared <- intersect(colnames(sc), colnames(sp))
    drop_these <- unique(c(y, zcol, weight))
    vars <- setdiff(shared, drop_these)

    if (length(vars) == 0L) {
      stop("No shared covariates found to build default p_formula.", call. = FALSE)
    }

    fml <- as.formula(paste("~", paste(vars, collapse = " + ")))
    msg <- paste0("Generated default p_formula: ", deparse(fml),"\n")
    log_messages <- c(log_messages, msg)
    if (verbose) message(msg)

    return(list(p_formula = fml, log_messages = log_messages))
  }

  # --- Multi-reference case ---
  else if (is.list(sp)) {
    p_formula_list <- vector("list", length(sp))

    for (i in seq_along(sp)) {
      vars_i <- setdiff(colnames(sp[[i]]), unique(c(y, zcol, weight[[i]])))

      if (length(vars_i) == 0L) {
        stop(sprintf("No valid covariates found to build default p_formula for reference %s.",
                     names(sp)[i] %||% paste0("[[", i, "]]")), call. = FALSE)
      }

      fml_i <- as.formula(paste("~", paste(vars_i, collapse = " + ")))
      p_formula_list[[i]] <- fml_i

      # --- choose readable name ---
      sp_name <- if (!is.null(names(sp)) && names(sp)[i] != "") names(sp)[i] else paste0("sp[[", i, "]]")

      msg <- sprintf("Generated default p_formula for %s: %s\n",
                     sp_name, deparse(fml_i))
      log_messages <- c(log_messages, msg)

      if (verbose) message(msg)
    }


    names(p_formula_list) <- if (!is.null(names(sp))) names(sp) else paste0("sp", seq_along(sp))

    return(list(p_formula = p_formula_list, log_messages = log_messages))
  }

}
