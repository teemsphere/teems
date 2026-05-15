skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset(), teardown_env())

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "example")
temp_dir <- file.path(write_dir, "tmp")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(temp_dir, recursive = TRUE)

GTAPv6_dir <- file.path(write_dir, "GTAPv6")
if (dir.exists(GTAPv6_dir)) {
  unlink(list.dirs(GTAPv6_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(GTAPv6_dir, recursive = TRUE)
}

GTAPv7_dir <- file.path(write_dir, "GTAPv7")
if (dir.exists(GTAPv7_dir)) {
  unlink(list.dirs(GTAPv7_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(GTAPv7_dir, recursive = TRUE)
}

GTAP_RE_dir <- file.path(write_dir, "GTAP-RE")
if (dir.exists(GTAP_RE_dir)) {
  unlink(list.dirs(GTAP_RE_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(GTAP_RE_dir, recursive = TRUE)
}

GTAP_INT_dir <- file.path(write_dir, "GTAP-INT")
if (dir.exists(GTAP_INT_dir)) {
  unlink(list.dirs(GTAP_INT_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(GTAP_INT_dir, recursive = TRUE)
}

variant <- Sys.info()["sysname"]

test_that("ems_example errors when model is missing", {
  expect_snapshot_error(ems_example())
})

test_that("ems_example errors when write_dir parent dir does not exist", {
  write_dir <- file.path(temp_dir, "parent_dir", "nonexistent_dir_xyz")
  parent_dir <- dirname(write_dir)
  if (dir.exists(parent_dir)) {
    unlink(parent_dir, recursive = TRUE)
  }
  expect_snapshot_error(
    ems_example("GTAPv7", write_dir = write_dir),
    variant = variant
  )
})

test_that("ems_example warns when write_dir does not exist", {
  write_dir <- file.path(temp_dir, "nonexistent_dir_xyz")
  if (dir.exists(write_dir)) {
    unlink(write_dir)
  }
  expect_snapshot_warning(
    ems_example("GTAPv7", write_dir = write_dir),
    variant = variant
  )
})

test_that("ems_example errors when type is scripts and dat_input is missing", {
  expect_snapshot_error(
    ems_example("GTAPv7",
      write_dir = write_dir, type = "scripts",
      par_input = par_input, set_input = set_input
    )
  )
})

test_that("ems_example errors when type is scripts and par_input is missing", {
  expect_snapshot_error(
    ems_example("GTAPv7",
      write_dir = write_dir, type = "scripts",
      dat_input = dat_input, set_input = set_input
    )
  )
})

test_that("ems_example errors when type is scripts and set_input is missing", {
  expect_snapshot_error(
    ems_example("GTAPv7",
      write_dir = write_dir, type = "scripts",
      dat_input = dat_input, par_input = par_input
    )
  )
})

test_that("ems_example returns model_file path for GTAPv7", {
  result <- ems_example("GTAPv7", write_dir = write_dir)
  expect_true("model_file" %in% names(result))
  expect_true(file.exists(result[["model_file"]]))
})

test_that("ems_example returns closure_file path for GTAPv7", {
  result <- ems_example("GTAPv7", write_dir = write_dir)
  expect_true("closure_file" %in% names(result))
  expect_true(file.exists(result[["closure_file"]]))
})

test_that("ems_example model_file is a .tab file", {
  result <- ems_example("GTAPv7", write_dir = write_dir)
  expect_true(grepl("\\.tab$", result[["model_file"]]))
})

test_that("ems_example closure_file is a .cls file", {
  result <- ems_example("GTAPv7", write_dir = write_dir)
  expect_true(grepl("\\.cls$", result[["closure_file"]]))
})

test_that("ems_example GTAPv6 scripts run without errors", {
  scripts <- ems_example(
    "GTAPv6",
    "scripts",
    dat_input = Sys.getenv("GTAP10A_dat"),
    par_input = Sys.getenv("GTAP10A_par"),
    set_input = Sys.getenv("GTAP10A_set"),
    GTAPv6_dir
  )
  
  checks <- lapply(scripts, source, local = TRUE)
  checks <- unlist(lapply(checks, \(r) {r$value}))
  expect_all_true(checks)
})

test_that("ems_example GTAPv7 scripts run without errors", {
  scripts <- ems_example(
    "GTAPv7",
    "scripts",
    dat_input,
    par_input,
    set_input,
    GTAPv7_dir
  )

  checks <- lapply(scripts, source, local = TRUE)
  checks <- unlist(lapply(checks, \(r) {r$value}))
  expect_all_true(checks)
})

test_that("ems_example GTAP-INT scripts run without errors", {
  scripts <- ems_example(
    "GTAP-INT",
    "scripts",
    dat_input = Sys.getenv("GTAP10A_dat"),
    par_input = Sys.getenv("GTAP10A_par"),
    set_input = Sys.getenv("GTAP10A_set"),
    GTAP_INT_dir
  )

  checks <- lapply(scripts, source, local = TRUE)
  checks <- unlist(lapply(checks, \(r) {r$value}))
  expect_all_true(checks)
})

test_that("ems_example GTAP-RE scripts run without errors", {
  scripts <- ems_example(
    "GTAP-RE",
    "scripts",
    dat_input,
    par_input,
    set_input,
    GTAP_RE_dir
  )

  checks <- lapply(scripts, source, local = TRUE)
  checks <- unlist(lapply(checks, \(r) {r$value}))
  expect_all_true(checks)
})

unlink(tools::R_user_dir("teems", "cache"), recursive = TRUE)