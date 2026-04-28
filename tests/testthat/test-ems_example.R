skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

temp_dir <- withr::local_tempdir()

data_dir <- "~/src/teems/teems_dat"
dat_input <- file.path(data_dir, "gsdfdat.har")
par_input <- file.path(data_dir, "gsdfpar.har")
set_input <- file.path(data_dir, "gsdfset.har")

# --- error tests ---

test_that("ems_example errors when model is missing", {
  expect_snapshot_error(ems_example())
})

test_that("ems_example warns when write_dir does not exist", {
  write_dir <- file.path(tools::R_user_dir("teems", "cache"), "nonexistent_dir_xyz")
  if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
  }
  expect_snapshot_warning(
    ems_example("GTAPv7", write_dir = write_dir)
  )
})

test_that("ems_example errors when type is scripts and dat_input is missing", {
  expect_snapshot_error(
    ems_example("GTAPv7", write_dir = temp_dir, type = "scripts",
                par_input = par_input, set_input = set_input)
  )
})

test_that("ems_example errors when type is scripts and par_input is missing", {
  expect_snapshot_error(
    ems_example("GTAPv7", write_dir = temp_dir, type = "scripts",
                dat_input = dat_input, set_input = set_input)
  )
})

test_that("ems_example errors when type is scripts and set_input is missing", {
  expect_snapshot_error(
    ems_example("GTAPv7", write_dir = temp_dir, type = "scripts",
                dat_input = dat_input, par_input = par_input)
  )
})

# --- acceptance tests: model_files type ---

test_that("ems_example returns model_file path for GTAPv7", {
  result <- ems_example("GTAPv7", write_dir = temp_dir)
  expect_true("model_file" %in% names(result))
  expect_true(file.exists(result[["model_file"]]))
})

test_that("ems_example returns closure_file path for GTAPv7", {
  result <- ems_example("GTAPv7", write_dir = temp_dir)
  expect_true("closure_file" %in% names(result))
  expect_true(file.exists(result[["closure_file"]]))
})

test_that("ems_example model_file is a .tab file", {
  result <- ems_example("GTAPv7", write_dir = temp_dir)
  expect_true(grepl("\\.tab$", result[["model_file"]]))
})

test_that("ems_example closure_file is a .cls file", {
  result <- ems_example("GTAPv7", write_dir = temp_dir)
  expect_true(grepl("\\.cls$", result[["closure_file"]]))
})

test_that("ems_example returns list for GTAPv6", {
  result <- ems_example("GTAPv6", write_dir = temp_dir)
  expect_type(result, "character")
  expect_true(file.exists(result[["model_file"]]))
})

test_that("ems_example returns list for GTAPv7", {
  result <- ems_example("GTAPv7", write_dir = temp_dir)
  expect_type(result, "character")
  expect_true(file.exists(result[["model_file"]]))
})

test_that("ems_example returns list for GTAP-INT", {
  result <- ems_example("GTAP-INT", write_dir = temp_dir)
  expect_type(result, "character")
  expect_true(file.exists(result[["model_file"]]))
})

test_that("ems_example returns list for GTAP-RE", {
  result <- ems_example("GTAP-RE", write_dir = temp_dir)
  expect_type(result, "character")
  expect_true(file.exists(result[["model_file"]]))
})

# --- acceptance tests: scripts type ---

test_that("ems_example scripts type returns list of script paths for GTAPv7", {
  result <- ems_example(
    "GTAPv7",
    write_dir = temp_dir,
    type = "scripts",
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input
  )
  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("ems_example scripts type writes files that exist", {
  result <- ems_example(
    "GTAPv7",
    write_dir = temp_dir,
    type = "scripts",
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input
  )
  expect_true(all(file.exists(unlist(result))))
})
