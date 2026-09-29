local_cols <- function(values, labels, kind = 1L, listed = TRUE, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  prefix <- file.path(dir, "sol.")
  nrow <- nrow(values)
  ncol <- ncol(values)
  con <- file(paste0(prefix, "cols"), "wb")
  writeBin(as.integer(c(1L, ncol, nrow, kind)), con, size = 8L, endian = "little")
  writeBin(as.integer(seq_len(nrow) - 1L), con, size = 8L, endian = "little")
  writeBin(as.double(values), con, size = 8L, endian = "little")
  close(con)
  kinds <- c("mixed", "subtotal", "sagem_individual", "approx_cumulative", "pass_solution")
  columns <- data.frame(
    index = seq_len(ncol) - 1L, kind = kinds[kind + 1L],
    label = labels, shocked = rep("aoall", ncol)
  )
  jsonlite::write_json(
    list(version = 1L, ncol = ncol, nrow = nrow, kind = kinds[kind + 1L], rows = "all", columns = columns),
    paste0(prefix, "cols.json"),
    auto_unbox = TRUE
  )
  files <- if (listed) c("sol.bin", "sol.cols", "sol.cols.json") else "sol.bin"
  writeBin(raw(8), paste0(prefix, "bin"))
  jsonlite::write_json(
    list(version = 1L, complete = TRUE, files = data.frame(name = files, kind = files)),
    paste0(prefix, "outputs.json"),
    auto_unbox = TRUE
  )
  prefix
}

test_that("subtotal columns are read with their labels and rows in .bin order", {
  values <- cbind(c(1.5, -2, 0), c(0.25, 3, 1e-300))
  prefix <- local_cols(values, c("china productivity", "trade \"and\" population"))
  cols <- .parse_solution_cols(prefix)
  expect_identical(cols$kind, "subtotal")
  expect_identical(cols$rows, 0:2)
  expect_identical(cols$values, values)
  expect_identical(cols$columns$label, c("china productivity", "trade \"and\" population"))
  expect_identical(cols$columns$kind, rep("subtotal", 2))
})

test_that("Johansen shock-group columns read as SAGEM individual columns", {
  prefix <- local_cols(matrix(c(1, 2), ncol = 1), "aoall(\"crops\",\"chn\")", kind = 2L)
  expect_identical(.parse_solution_cols(prefix)$kind, "sagem_individual")
})

test_that("columns a run did not list as complete are not read", {
  prefix <- local_cols(matrix(1, 1, 1), "g", listed = FALSE)
  expect_null(.parse_solution_cols(prefix))
  prefix <- local_cols(matrix(1, 1, 1), "g")
  unlink(paste0(prefix, "outputs.json"))
  expect_null(.parse_solution_cols(prefix))
})

test_that("a column file that disagrees with its index is not read", {
  prefix <- local_cols(matrix(1:4 / 2, 2, 2), c("a", "b"))
  index <- jsonlite::fromJSON(paste0(prefix, "cols.json"))
  index$nrow <- 3L
  jsonlite::write_json(index, paste0(prefix, "cols.json"), auto_unbox = TRUE)
  expect_null(.parse_solution_cols(prefix))
})
