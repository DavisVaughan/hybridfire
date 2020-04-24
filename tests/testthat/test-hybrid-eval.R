# ------------------------------------------------------------------------------
# variable evaluation

test_that("can eval literals", {
  expect_identical(hybrid_eval(1), list(1))
  expect_identical(hybrid_eval("x"), list("x"))
})

test_that("can eval variables in the surrounding env", {
  x <- 1
  expr <- expr(x)

  expect_identical(hybrid_eval(expr), list(1))
})

test_that("can eval variables from hybrid mask", {
  col <- hybrid_list(1:2, 3:5)
  mask <- list(a = col)
  expr <- expr(a)

  expect_identical(hybrid_eval(expr, mask), col)
})

test_that("hybrid mask takes precedence over env", {
  x <- 1
  col <- hybrid_list(1:2)
  mask <- list(x = col)
  expr <- expr(x)

  expect_identical(hybrid_eval(expr, mask), col)
})

# ------------------------------------------------------------------------------
# standard call eval

test_that("can eval standard call with literals", {
  expr <- expr(plus(1, 1))
  expect_identical(hybrid_eval(expr), list(2))

  expr <- expr(plus(plus(1, 1), 1))
  expect_identical(hybrid_eval(expr), list(3))
})

test_that("can eval standard call with hybrid mask", {
  col_a <- hybrid_list(1:2, 3:5)
  col_b <- hybrid_list(6:7, 8:10)
  mask <- list(a = col_a, b = col_b)

  expr <- expr(plus(a, b))

  expect <- list(
    col_a[[1]] + col_b[[1]],
    col_a[[2]] + col_b[[2]]
  )

  expect_identical(hybrid_eval(expr, mask), expect)
})

test_that("can mix hybrid mask with literals", {
  col <- hybrid_list(1:2, 3:5)
  mask <- list(a = col)

  expr <- expr(plus(a, 1))

  expect <- map(col, function(x) x + 1)

  expect_identical(hybrid_eval(expr, mask), expect)
})

# ------------------------------------------------------------------------------
# hybrid eval

test_that("grouped_mean() can be supplied directly", {
  col <- hybrid_list(1:2, 3:5)
  mask <- list(a = col)

  expr <- expr(grouped_mean(a))

  expect <- map(col, mean)

  expect_identical(hybrid_eval(expr, mask), expect)

  # no signaling here, nothing got replaced
  expect_null(catch_cnd(hybrid_eval(expr, mask)))
})

test_that("mean() is hybridized with 1 mask column", {
  col <- hybrid_list(1:2, 3:5)
  mask <- list(a = col)

  expr <- expr(mean(a))

  expect <- map(col, mean)

  expect_identical(hybrid_eval(expr, mask), expect)

  expect_signal_hybrid_replaced(hybrid_eval(expr, mask))
})

test_that("mean() is hybridized with outside variable", {
  x <- 1:5

  expr <- expr(mean(x))

  expect <- map(list(x), mean)

  expect_identical(hybrid_eval(expr), expect)

  expect_signal_hybrid_replaced(hybrid_eval(expr))
})

test_that("mean() is not hybridized with `trim` specified", {
  col <- hybrid_list(1:2, c(3, 2, 6, 10))
  mask <- list(a = col)

  expr <- expr(mean(a, trim = .5))

  expect <- map(col, mean, trim = .5)

  expect_identical(hybrid_eval(expr, mask), expect)

  expect_no_signal_hybrid_replaced(hybrid_eval(expr, mask))
})

test_that("mean() is not hybridized on S3 objects", {
  x <- as.Date("2019-01-01")
  y <- x + 1:2

  col <- hybrid_list(x, y)
  mask <- list(a = col)

  expr <- expr(mean(a))

  expect <- map(col, mean)

  expect_identical(hybrid_eval(expr, mask), expect)

  expect_no_signal_hybrid_replaced(hybrid_eval(expr, mask))
})

test_that("mean() is hybridized when mixed with other mean() calls", {
  col <- hybrid_list(1:2, 3:5)
  mask <- list(a = col)

  expr <- expr(mean(mean(a)))

  expect_signal_hybrid_replaced(hybrid_eval(expr, mask))
})

test_that("mean() is hybridized when mixed with non hybridized calls", {
  col_a <- hybrid_list(1:2, 3:5)
  col_b <- hybrid_list(6:7, 8:10)
  mask <- list(a = col_a, b = col_b)

  expr <- expr(mean(a + b) + a)

  expect <- map2(col_a, col_b, ~mean(.x + .y) + .x)

  expect_identical(hybrid_eval(expr, mask), expect)

  expect_signal_hybrid_replaced(hybrid_eval(expr, mask))
})
