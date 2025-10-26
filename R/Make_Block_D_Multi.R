make_block_D <- function(sp_list, wts_cols, xcol) {
  # sp_list   : list of data.frames (sp1, sp2, …)
  # wts_cols  : character vector of the weight‐column names, same length as sp_list
  # xcol      : list of character vectors of covariate names for each sample
  
  n <- length(sp_list)
  
  # 1) determine block sizes (first block gets an intercept)
  block_sizes <- sapply(seq_len(n), function(i) {
    if (i == 1) {
      length(xcol[[i]]) + 1
    } else {
      length(xcol[[i]])
    }
  })
  total_dim <- sum(block_sizes)
  
  # 2) compute block boundaries
  starts <- cumsum(c(1, head(block_sizes, -1)))
  ends   <- cumsum(block_sizes)
  
  # 3) initialize D
  D <- matrix(0, total_dim, total_dim)
  
  # 4) fill each diagonal block
  for (i in seq_len(n)) {
    sp  <- sp_list[[i]]
    wts <- sp[[ wts_cols[i] ]]
    
    # build design matrix for this sample
    if (i == 1) {
      Xp <- design_matrix( xcol[[i]],sp, intercept = T)
    } else {
      Xp <- design_matrix( xcol[[i]],sp, intercept = F)
    }
    
    # Poisson sampling variance factor
    c_i <- wts^2 * (1 - 1 / wts)
    
    # sample‐specific D_i
    D_i <- t(c_i * Xp) %*% Xp
    
    # insert into the big D
    idx <- starts[i]:ends[i]
    D[idx, idx] <- D_i
  }
  
  return(D)
}

