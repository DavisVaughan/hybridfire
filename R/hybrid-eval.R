#' Evaluate an expression with hybrid evaluation
#'
#' `hybrid_eval()` iterates through the AST of `expr`, evaluating each
#' component in a way that is convenient for mixing hybrid eval with standard
#' eval.
#'
#' - If `expr` a call, we extract out the function with `node_car()`, and
#'   the arguments with `node_cdr()`.
#'
#'   - The `fn` is really a name at this point, so we evaluate it in the
#'     `env` to get the real function back.
#'
#'   - The `args` are mapped over, and `hybrid_eval()` is called on each one.
#'
#'   - We then use `hybrid_replace()` to check if we can replace the standard
#'     `fn` with a hybrid version. i.e. can we replace `mean(x)` with
#'     `grouped_mean(x)`?
#'
#'   - If the function gets replaced (or `grouped_mean()` was supplied
#'     directly), it is considered "hybridizable". A hybridizable function
#'     like `grouped_mean(x)` expects `x` as a list of chunks, so we can
#'     just call it directly. Otherwise we have standard eval and we
#'     `pmap()` over all the args, calling `fn` on each group.
#'
#' @param expr
#'
#'   An expression to evaluate.
#'
#' @param mask
#'
#'   A named list. The names match the columns in the data frame. A single
#'   element of this list is also a list, corresponding to the chunks for
#'   that particular column.
#'
#' @param env
#'
#'   The environment where `expr` was specified.
#'
hybrid_eval <- function(expr, mask = list(), env = caller_env()) {
  if (is.call(expr)) {
    hybrid_eval_call(expr, mask, env)
  } else if (is.symbol(expr)) {
    hybrid_eval_symbol(expr, mask, env)
  } else {
    hybrid_eval_literal(expr, mask, env)
  }
}

hybrid_eval_call <- function(expr, mask, env) {
  fn <- node_car(expr)
  fn <- eval_bare(fn, env = env)

  args <- node_cdr(expr)
  args <- purrr::map(args, hybrid_eval, mask = mask, env = env)

  fn <- hybrid_replace(fn, args)

  if (is_hybridizable(fn)) {
    out <- exec(fn, !!!args)
  } else {
    out <- purrr::pmap(args, fn)
  }

  out
}

hybrid_eval_symbol <- function(expr, mask, env) {
  # Try getting it from the data frame chunk mask
  # Use fast symbol subsetting here
  out <- mask[[expr]]

  # If it matches a column name in our data frame, return that column
  if (!is.null(out)) {
    return(out)
  }

  # Otherwise, try evaluating in the `env`
  out <- eval_bare(expr, env)

  # Wrap in a list to match "chunk" result style
  out <- list(out)

  out
}

hybrid_eval_literal <- function(expr, mask, env) {
  # Wrap in a list to match "chunk" result style
  list(expr)
}
