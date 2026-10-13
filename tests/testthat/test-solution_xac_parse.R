local_xac <- function(passes, codes, listed = TRUE, nsubints = 1L, version = 2L,
                      error = NULL, levels = NULL, flags = NULL, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  prefix <- file.path(dir, "sol.")
  nrow <- nrow(passes)
  con <- file(paste0(prefix, "xac"), "wb")
  writeBin(as.integer(c(version, nrow, 3L, nsubints)), con, size = 8L, endian = "little")
  writeBin(as.double(passes), con, size = 8L, endian = "little")
  if (identical(version, 1L)) {
    writeBin(as.integer(codes), con, size = 4L, endian = "little")
  } else {
    writeBin(as.double(error), con, size = 8L, endian = "little")
    writeBin(as.integer(codes), con, size = 1L, endian = "little")
    writeBin(as.integer(levels), con, size = 1L, endian = "little")
    writeBin(integer(nrow), con, size = 1L, endian = "little")
    writeBin(as.integer(flags), con, size = 1L, endian = "little")
  }
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
xac_codes <- c(6L, 5L, 7L, 3L, 0L)
xac_error <- c(1e-7, 2e-6, 0, 3e-3, 0.4)
xac_levels <- c(6L, 6L, 6L, 5L, 1L)
xac_flags <- c(1L, 1L, 5L, 2L, 9L)
local_xac2 <- function(env = parent.frame(), ...) {
  local_xac(xac_passes, xac_codes,
    error = xac_error, levels = xac_levels, flags = xac_flags, env = env, ...
  )
}

test_that("pass solutions and accuracy columns are read for every variable in .bin order", {
  prefix <- local_xac2()
  vars <- data.frame(begadd = c(0, 2), matsize = c(2, 3))
  xac <- .parse_solution_xac(prefix, vars)
  expect_identical(
    names(xac),
    c("Pass1", "Pass2", "Pass3", "Error", "Figures", "LevelsFigures", "Summarised")
  )
  expect_identical(xac$Pass1, xac_passes[, 1])
  expect_identical(xac$Pass3, xac_passes[, 3])
  expect_identical(xac$Error, xac_error)
  expect_identical(xac$Figures, c(6L, 5L, NA, 3L, 0L))
  expect_identical(xac$LevelsFigures, xac_levels)
  expect_identical(xac$Summarised, c(TRUE, TRUE, TRUE, FALSE, TRUE))
})

test_that("a version 1 pass file from an older solver is still read", {
  prefix <- local_xac(xac_passes, c(6L, 5L, 4L, 3L, 1L), version = 1L)
  xac <- .parse_solution_xac(prefix, data.frame(begadd = 0, matsize = 5))
  expect_identical(xac$Figures, c(6L, 5L, 4L, 3L, 1L))
  expect_true(all(is.na(xac$LevelsFigures)))
  expect_true(all(is.na(xac$Error)))
})

test_that("a selection reads only the selected variables' rows", {
  prefix <- local_xac2()
  xac <- .parse_solution_xac(prefix, data.frame(begadd = 2, matsize = 3))
  expect_identical(xac$Pass2, xac_passes[3:5, 2])
  expect_identical(xac$Error, xac_error[3:5])
  expect_identical(xac$LevelsFigures, xac_levels[3:5])
})

test_that("a pass file the run did not list as complete is not read", {
  prefix <- local_xac2(listed = FALSE)
  expect_null(.parse_solution_xac(prefix, data.frame(begadd = 0, matsize = 5)))
})

test_that("a truncated pass file is not read", {
  prefix <- local_xac2()
  bytes <- readBin(paste0(prefix, "xac"), "raw", n = 1e4)
  writeBin(utils::head(bytes, -4L), paste0(prefix, "xac"))
  expect_null(.parse_solution_xac(prefix, data.frame(begadd = 0, matsize = 5)))
})
