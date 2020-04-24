expect_signal_hybrid_replaced <- function(expr) {
  expect_s3_class(
    catch_cnd(expr, classes = "hybrid_replaced"),
    "hybrid_replaced"
  )
}

expect_no_signal_hybrid_replaced <- function(expr) {
  expect_null(catch_cnd(expr, classes = "hybrid_replaced"))
}
