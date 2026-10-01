test_that("levels of a percent-change variable: post = pre (1 + x/100)", {
  dt <- data.table::data.table(REGr = c("a", "b"), Value = c(10, -50))
  out <- .add_levels(dt, c(200, 4))
  expect_identical(out$PreLevel, c(200, 4))
  expect_equal(out$PostLevel, c(220, 2))
  expect_equal(out$Change, c(20, -2))
  expect_false("PercentChange" %in% names(out))
})

test_that("levels of a change variable: post = pre + x, and the percent change", {
  dt <- data.table::data.table(REGr = c("a", "b"), Value = c(3, 1))
  pre <- c(30, 0)
  attr(pre, "change") <- TRUE
  out <- .add_levels(dt, pre)
  expect_equal(out$PostLevel, c(33, 1))
  expect_equal(out$PercentChange, c(10, NA))
  expect_false("Change" %in% names(out))
})

test_that("the ORIG_LEVEL target is read from a qualifier list", {
  expect_identical(
    .orig_level_target(c("(orig_level=VDFB)", "(change)", "(orig_level = 1.0, change)", NA)),
    c("vdfb", NA, "1.0", NA)
  )
})
