local_jac <- function(row, col, value, nrow = 3L, ncol = 4L, listed = TRUE, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  prefix <- file.path(dir, "sol.")
  nnz <- length(value)
  con <- file(paste0(prefix, "jac"), "wb")
  writeBin(as.integer(c(1L, nrow, ncol, nnz)), con, size = 8L, endian = "little")
  writeBin(as.integer(row), con, size = 8L, endian = "little")
  writeBin(as.integer(col), con, size = 8L, endian = "little")
  writeBin(as.double(value), con, size = 8L, endian = "little")
  close(con)
  equations <- data.frame(name = c("e_a", "e_b"), first_row = c(0L, 2L), nrows = c(2L, 1L))
  equations$sets <- list(c("reg"), character(0))
  jsonlite::write_json(
    list(version = 1L, point = "base", nrow = nrow, ncol = ncol, nnz = nnz, equations = equations),
    paste0(prefix, "jac.json"),
    auto_unbox = TRUE
  )
  files <- if (listed) c("sol.jac", "sol.jac.json") else "sol.bin"
  jsonlite::write_json(
    list(version = 1L, complete = TRUE, files = data.frame(name = files, kind = files)),
    paste0(prefix, "outputs.json"),
    auto_unbox = TRUE
  )
  prefix
}

test_that("the base-point Jacobian is read as triplets with its row map", {
  prefix <- local_jac(c(0L, 0L, 1L, 2L), c(0L, 3L, 1L, 2L), c(1.5, -1, 1e-300, 2))
  jac <- .parse_jacobian(prefix)
  expect_identical(jac$nrow, 3L)
  expect_identical(jac$ncol, 4L)
  expect_identical(jac$row, c(0L, 0L, 1L, 2L))
  expect_identical(jac$col, c(0L, 3L, 1L, 2L))
  expect_identical(jac$value, c(1.5, -1, 1e-300, 2))
  expect_identical(jac$equations$name, c("e_a", "e_b"))
  expect_identical(jac$equations$first_row, c(0L, 2L))
  expect_identical(jac$equations$sets[[1]], "reg")
})

test_that("a Jacobian the run did not list as complete is not read", {
  prefix <- local_jac(0L, 0L, 1, listed = FALSE)
  expect_null(.parse_jacobian(prefix))
  prefix <- local_jac(0L, 0L, 1)
  unlink(paste0(prefix, "outputs.json"))
  expect_null(.parse_jacobian(prefix))
})

test_that("a Jacobian file that disagrees with its index is not read", {
  prefix <- local_jac(c(0L, 1L), c(0L, 1L), c(1, 2))
  index <- jsonlite::fromJSON(paste0(prefix, "jac.json"))
  index$nnz <- 3L
  jsonlite::write_json(index, paste0(prefix, "jac.json"), auto_unbox = TRUE)
  expect_null(.parse_jacobian(prefix))
})
