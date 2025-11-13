sort_by_sp_size <- function(sp, vars, weight, verbose = FALSE) {
  log_messages <- character()  # store printed messages

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
  sorted_sizes <- sizes[idx]

  # compose formatted message
  msg_lines <- paste0("  ", sorted_names, " (n = ", sorted_sizes, ")")
  msg <- paste0(
    "\nReference samples summary:\nOrder of samples by size (largest to smallest):\n",
    paste(msg_lines, collapse = "\n"), "\n"
  )

  log_messages <- c(log_messages, msg)
  if (verbose) cat(msg)

  # assign sorted names to new list
  names(sp_sorted) <- sorted_names

  # return sorted results
  list(
    sp     = sp_sorted,
    vars   = vars_sorted,
    weight = weight_sorted,
    log    = log_messages
  )
}
