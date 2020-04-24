#' Is `x` a hybrid list?
#'
#' A hybrid list is a wrapper around the list of chunks that make up a single
#' chunked column.
is_hybrid_list <- function(x) {
  inherits(x, "hybrid_list")
}

hybrid_list <- function(...) {
  out <- list2(...)
  new_hybrid_list(out)
}

new_hybrid_list <- function(x) {
  structure(x, class = "hybrid_list")
}

# We need to know if our chunked column is a list of S3 objects, or a list
# of simple atomics
is_hybrid_list_of_atomics <- function(x) {
  if (length(x) == 0L) {
    return(TRUE)
  }

  x <- x[[1L]]

  !is.object(x)
}

any_hybrid_lists <- function(x) {
  any(purrr::map_lgl(x, is_hybrid_list))
}
