Xc_Xp_Construction <- function(vars_XC, sc, sp_list, xcol, wts_cols) {
  # build design matrix for convenience sample (with intercept)
  Xc <- design_matrix(vars_XC, data = sc, intercept = TRUE)
  
  # build design matrices & weight vectors for each reference sample
  n_ref    <- length(sp_list)
  Xp_list  <- vector("list", n_ref)
  wts_list <- vector("list", n_ref)
  
  for (i in seq_len(n_ref)) {
    Xp_list[[i]] <- design_matrix(
      xcol[[i]],
      data      = sp_list[[i]],
      intercept = (i == 1)
    )
    wts_list[[i]] <- sp_list[[i]][[ wts_cols[i] ]]
  }
  
  return(list(
    Xc       = Xc,
    Xp_list  = Xp_list,
    wts_list = wts_list
  ))
}

