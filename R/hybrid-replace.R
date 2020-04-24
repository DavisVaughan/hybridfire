
hybrid_replace <- function(fn, args) {
  if (is_replaceable_mean(fn, args)) {
    signal_hybrid_replaced()
    return(grouped_mean)
  }

  fn
}

# ------------------------------------------------------------------------------

#' Is this a simple mean call?
#'
#' Yes
#' mean(hybrid_list)
#'
#' Yes
#' mean(hybrid_list, na.rm = TRUE)
#'
#' No
#' mean(S3)
#'
#' No
#' mean(hybrid_list_of_s3)
#'
#' No
#' mean(hybrid_list, hybrid_list)
is_replaceable_mean <- function(fn, args) {
  if (!identical(fn, base::mean)) {
    return(FALSE)
  }

  # na.rm is allowed
  args$na.rm <- NULL

  # Only allowed to replace `mean(x)` or `mean(x, na.rm = TRUE/FALSE)`
  if (length(args) != 1L) {
    return(FALSE)
  }

  arg <- args[[1L]]

  if (is_hybrid_list(arg)) {
    is_hybrid_list_of_atomics(arg)
  } else if (is.object(arg)) {
    FALSE
  } else {
    TRUE
  }
}
