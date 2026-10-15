cof_record <- function(name, begadd, matsize) {
  rec <- raw(456L)
  chars <- charToRaw(name)
  rec[seq_along(chars)] <- chars
  rec[257:264] <- writeBin(as.integer(begadd), raw(), size = 8L, endian = "little")
  rec[265:268] <- writeBin(1L, raw(), size = 4L, endian = "little")
  rec[273:280] <- writeBin(1L, raw(), size = 8L, endian = "little")
  rec[353:360] <- writeBin(1L, raw(), size = 8L, endian = "little")
  rec[433:440] <- writeBin(as.integer(matsize), raw(), size = 8L, endian = "little")
  rec
}

write_solution <- function(dir, values, nvarele = length(values)) {
  prefix <- file.path(dir, "sol.")
  writeBin(as.integer(c(0L, 1L, nvarele, 0L)), paste0(prefix, "mds"), size = 8L, endian = "little")
  writeBin(cof_record("x", 0L, length(values)), paste0(prefix, "var"))
  writeBin(as.double(values), paste0(prefix, "bin"), size = 8L, endian = "little")
  file.create(paste0(prefix, "sel"), paste0(prefix, "set"))
  prefix
}

write_coefficients <- function(dir, values, ncofele = length(values)) {
  prefix <- file.path(dir, "sol.")
  cof <- c(
    writeBin(as.integer(c(1L, 1L, ncofele, 0L)), raw(), size = 8L, endian = "little"),
    cof_record("c", 0L, length(values)),
    as.raw(0L)
  )
  writeBin(cof, paste0(prefix, "cof"))
  writeBin(as.double(values), paste0(prefix, "cbin"), size = 8L, endian = "little")
  prefix
}

truncate_file <- function(path, n_bytes) {
  bytes <- readBin(path, "raw", n = file.size(path))
  writeBin(utils::head(bytes, n_bytes), path)
}

test_that("a consistent solution round-trips through the binary parsers", {
  dir <- withr::local_tempdir()
  values <- c(1.5, -2, 3.25)
  prefix <- write_solution(dir, values)
  all_vars <- parse_solution_bins(prefix, character(0))
  expect_identical(all_vars$bin, values)
  expect_identical(all_vars$var$cofname, "x")
  one_var <- parse_solution_bins(prefix, "x")
  expect_identical(one_var$bin, values)
  meta <- parse_solution_meta(prefix)
  expect_identical(meta$var$cofname, "x")
  expect_identical(meta$var$matsize, 3)
})

test_that("a short .mds aborts instead of sizing allocations from garbage", {
  dir <- withr::local_tempdir()
  prefix <- write_solution(dir, c(1, 2))
  truncate_file(paste0(prefix, "mds"), 16L)
  expect_error(parse_solution_bins(prefix, character(0)), "Truncated .mds file")
  expect_error(parse_solution_meta(prefix), "Truncated .mds file")
})

test_that("a short .var aborts instead of returning empty records", {
  dir <- withr::local_tempdir()
  prefix <- write_solution(dir, c(1, 2))
  truncate_file(paste0(prefix, "var"), 100L)
  expect_error(parse_solution_bins(prefix, character(0)), "Truncated .var file")
  expect_error(parse_solution_meta(prefix), "Truncated .var file")
})

test_that("an element count that disagrees between .mds and .var aborts", {
  dir <- withr::local_tempdir()
  prefix <- write_solution(dir, c(1, 2), nvarele = 5L)
  expect_error(parse_solution_bins(prefix, character(0)), "Element count mismatch")
})

test_that("a short .bin or .est aborts", {
  dir <- withr::local_tempdir()
  prefix <- write_solution(dir, c(1, 2, 3))
  writeBin(c(0.1, 0.2), paste0(prefix, "est"), size = 8L, endian = "little")
  expect_error(parse_solution_bins(prefix, character(0)), "Truncated error-estimate file")
  truncate_file(paste0(prefix, "bin"), 8L)
  expect_error(parse_solution_bins(prefix, character(0)), "Truncated or empty .bin file")
})

test_that("coefficient binaries round-trip and reject truncation and mismatch", {
  dir <- withr::local_tempdir()
  values <- c(4, 5, 6, 7)
  prefix <- write_coefficients(dir, values)
  cof <- parse_coefficients(prefix, character(0), TRUE, FALSE)
  expect_identical(cof$bin, values)
  expect_identical(cof$cof$cofname, "c")
  truncate_file(paste0(prefix, "cbin"), 16L)
  expect_error(parse_coefficients(prefix, character(0), TRUE, FALSE), "Truncated coefficient values file")
  prefix <- write_coefficients(dir, values, ncofele = 3L)
  expect_error(parse_coefficients(prefix, character(0), TRUE, FALSE), "Element count mismatch")
  truncate_file(paste0(prefix, "cof"), 200L)
  expect_error(parse_coefficients(prefix, character(0), FALSE, FALSE), "Truncated .cof file")
})

test_that("a levels bound is read as float or double per the run's stored precision", {
  dir <- withr::local_tempdir()
  prefix <- write_solution(dir, c(1, 2, 3))
  rec <- readBin(paste0(prefix, "var"), raw(), 456L)
  rec[445:448] <- writeBin(1L, raw(), size = 4L, endian = "little")
  rec[449:456] <- writeBin(1.5, raw(), size = 8L, endian = "little")
  writeBin(rec, paste0(prefix, "var"))
  no_record <- .parse_solution_meta(prefix)
  expect_identical(no_record$var_union$gltype, 1L)
  expect_false(identical(no_record$var_union$glval, 1.5))
  writeLines(
    c("{", "  \"options\": {", "    \"store_precision\": \"double\",", "    \"refine\": true", "  }", "}"),
    paste0(prefix, "stats.json")
  )
  double_run <- .parse_solution_meta(prefix)
  expect_identical(double_run$var_union$glval, 1.5)
  bins <- .parse_solution_bins(prefix, "x")
  expect_identical(bins$var$glval, 1.5)
  rec[449:452] <- writeBin(2.5, raw(), size = 4L, endian = "little")
  rec[453:456] <- raw(4L)
  writeBin(rec, paste0(prefix, "var"))
  writeLines("{ \"options\": { \"store_precision\": \"single\" } }", paste0(prefix, "stats.json"))
  single_run <- .parse_solution_meta(prefix)
  expect_identical(single_run$var_union$glval, 2.5)
})
