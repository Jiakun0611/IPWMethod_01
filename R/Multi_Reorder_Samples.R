sort_by_sp_size <- function(sp, vars, weight, verbose = FALSE) {
  log_messages <- character()  # 用于保存输出
  
  # if vars not provided, initialize as empty list
  if (length(vars) == 0) vars <- vector("list", length = length(sp))
  
  # compute row counts for each data.frame in sp
  sizes <- vapply(sp, NROW, integer(1))
  
  # get ordering index (largest to smallest)
  idx <- order(sizes, decreasing = TRUE)
  
  # reorder all three lists
  sp_sorted     <- sp[idx]
  vars_sorted   <- vars[idx]
  weight_sorted <- weight[idx]
  
  # prepare sorted names
  sp_names <- names(sp)
  if (is.null(sp_names)) {
    sp_names <- paste0("sp", seq_along(sp))
  }
  sorted_names <- sp_names[idx]
  
  msg <- paste0(
    "Order of sp by size (largest to smallest):\n  ",
    paste(sorted_names, collapse = "  "), "\n"
  )
  
  log_messages <- c(log_messages, msg)
  if (verbose) cat(msg)
  
  names(sp_sorted) <- sorted_names
  
  # return sorted lists + log
  list(
    sp     = sp_sorted,
    vars   = vars_sorted,
    weight = weight_sorted,
    log    = log_messages   
  )
}