test_that("whole-valued Real data keeps values beyond the integer range", {
  x <- c(514066944, 2431414016, -5729103360, 0)
  out <- .typed_values(x, "Real", 6L)
  expect_identical(out, x)
  expect_false(anyNA(out))
})

test_that("fractional Real data is rounded and Integer data is coerced", {
  expect_identical(.typed_values(c(1.23456789, 2), "Real", 6L), c(1.234568, 2))
  expect_identical(.typed_values(c(3, 7), "Integer", 6L), c(3L, 7L))
})
