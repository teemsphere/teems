# a single-HAR database (ems_data on one HAR file): character headers
# without repeats are sets; one with repeats is a by_elements mapping
# and has to stay readable by the mapping Read

test_that("a single-HAR character header with repeated entries is kept for mapping reads", {
  x <- list(
    REG = c("a", "b"),
    MAPC = c("g1", "g1", "g2"),
    XXHS = c("h", "h"),
    V = c(1.5, 2.5)
  )
  attr(x, "metadata") <- list(data_format = "GTAPv7")
  out <- .split_generic_headers(x)
  meta <- attr(out, "metadata")
  expect_identical(meta$set_names, "REG")
  expect_identical(meta$map_raw, list(MAPC = c("g1", "g1", "g2")))
  expect_false("MAPC" %in% names(out))
})
