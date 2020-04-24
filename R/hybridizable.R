#' Is a function hybridizable?
#'
#' Current a function is hybridizable if it is one of a list of
#' known functions.
#'
is_hybridizable <- function(x) {
  UseMethod("is_hybridizable")
}

is_hybridizable.default <- function(x) {
  FALSE
}

is_hybridizable.hybrid_function <- function(x) {
  TRUE
}
