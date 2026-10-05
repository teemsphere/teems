local_xac <- function(passes, codes, listed = TRUE, nsubints = 1L, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  prefix <- file.path(dir, "sol.")
  nrow <- nrow(passes)
  con <- file(paste0(prefix, "xac"), "wb")
  writeBin(as.integer(c(1L, nrow, 3L, nsubints)), con, size = 8L, endian = "little")
  writeBin(as.double(passes), con, size = 8L, endian = "little")
  writeBin(as.integer(codes), con, size = 4L, endian = "little")
  close(con)
  files <- if (listed) c("sol.bin", "sol.xac") else "sol.bin"
  writeBin(raw(8), paste0(prefix, "bin"))
  jsonlite::write_json(
    list(version = 1L, complete = TRUE, files = data.frame(name = files, kind = files)),
    paste0(prefix, "outputs.json"),
    auto_unbox = TRUE
  )
  prefix
}

xac_passes <- cbind(c(1, 2, 3, 4, 5), c(1.5, 2.5, 3.5, 4.5, 5.5), c(1.75, 2.75, 3.75, 4.75, 5.75))
xac_codes <- c(6L, 5L, 4L, 3L, 1L)

test_that("pass solutions and accuracy codes are read for every variable in .bin order", {
  prefix <- local_xac(xac_passes, xac_codes)
  vars <- data.frame(begadd = c(0, 2), matsize = c(2, 3))
  xac <- .parse_solution_xac(prefix, vars)
  expect_identical(names(xac), c("Pass1", "Pass2", "Pass3", "Accuracy"))
  expect_identical(xac$Pass1, xac_passes[, 1])
  expect_identical(xac$Pass3, xac_passes[, 3])
  expect_identical(xac$Accuracy, xac_codes)
})

test_that("a selection reads only the selected variables' rows", {
  prefix <- local_xac(xac_passes, xac_codes)
  xac <- .parse_solution_xac(prefix, data.frame(begadd = 2, matsize = 3))
  expect_identical(xac$Pass2, xac_passes[3:5, 2])
  expect_identical(xac$Accuracy, xac_codes[3:5])
})

test_that("a pass file the run did not list as complete is not read", {
  prefix <- local_xac(xac_passes, xac_codes, listed = FALSE)
  expect_null(.parse_solution_xac(prefix, data.frame(begadd = 0, matsize = 5)))
})

test_that("a truncated pass file is not read", {
  prefix <- local_xac(xac_passes, xac_codes)
  bytes <- readBin(paste0(prefix, "xac"), "raw", n = 1e4)
  writeBin(utils::head(bytes, -4L), paste0(prefix, "xac"))
  expect_null(.parse_solution_xac(prefix, data.frame(begadd = 0, matsize = 5)))
})
