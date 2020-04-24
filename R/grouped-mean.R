grouped_mean <- function(x, na.rm = FALSE) {
  if (!is.list(x)) {
    abort("`x` must be a list.")
  }

  if (na.rm) {
    x <- purrr::map(x, function(.x) .x[!is.na(.x)])
  }

  res <- purrr::map(x, function(.x) mean.default(.x))

  res
}

class(grouped_mean) <- c("hybrid_function", "function")
