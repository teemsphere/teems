skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "example")
temp_dir <- file.path(write_dir, "tmp")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(temp_dir, recursive = TRUE)
ems_option_set(
  verbose = FALSE,
  tempdir = write_dir
)
withr::defer(ems_option_reset(), teardown_env())

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

cvrtGTAPv6_dir <- file.path(write_dir, "cvrtGTAPv6")
if (dir.exists(cvrtGTAPv6_dir)) {
  unlink(list.dirs(cvrtGTAPv6_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(cvrtGTAPv6_dir, recursive = TRUE)
}

cvrtGTAPv7_dir <- file.path(write_dir, "cvrtGTAPv7")
if (dir.exists(cvrtGTAPv7_dir)) {
  unlink(list.dirs(cvrtGTAPv7_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(cvrtGTAPv7_dir, recursive = TRUE)
}

cvrtGTAP_RE_dir <- file.path(write_dir, "cvrtGTAP-RE")
if (dir.exists(cvrtGTAP_RE_dir)) {
  unlink(list.dirs(cvrtGTAP_RE_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(cvrtGTAP_RE_dir, recursive = TRUE)
}

cvrtGTAP_INT_dir <- file.path(write_dir, "cvrtGTAP-INT")
if (dir.exists(cvrtGTAP_INT_dir)) {
  unlink(list.dirs(cvrtGTAP_INT_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(cvrtGTAP_INT_dir, recursive = TRUE)
}
variant <- Sys.info()["sysname"]

test_that("ems_example errors when model is missing", {
  expect_snapshot_error(ems_example())
})

test_that("ems_example errors when path is missing", {
  expect_snapshot_error(ems_example("GTAPv7"))
})

test_that("ems_example errors when path does not exist", {
  expect_snapshot_error(
    ems_example("GTAPv7", file.path(write_dir, "not_a_dir")),
    variant = variant
  )
})

test_that("ems_example errors when type is scripts and an input is missing", {
  expect_snapshot_error(
    ems_example("GTAPv7", write_dir, "scripts", par_input = par_input, set_input = set_input)
  )
})

test_that("ems_example returns model_file path for GTAPv7", {
  result <- ems_example("GTAPv7", write_dir)
  expect_true("model_file" %in% names(result))
  expect_true(file.exists(result[["model_file"]]))
})

test_that("ems_example returns closure_file path for GTAPv7", {
  result <- ems_example("GTAPv7", write_dir)
  expect_true("closure_file" %in% names(result))
  expect_true(file.exists(result[["closure_file"]]))
})

test_that("ems_example model_file is a .tab file", {
  result <- ems_example("GTAPv7", write_dir)
  expect_true(grepl("\\.tab$", result[["model_file"]]))
})

test_that("ems_example closure_file is a .cls file", {
  result <- ems_example("GTAPv7", write_dir)
  expect_true(grepl("\\.cls$", result[["closure_file"]]))
})

test_that("ems_example GTAPv6 scripts run without errors", {
  scripts <- ems_example(
    "GTAPv6",
    GTAPv6_dir,
    "scripts",
    dat_input = Sys.getenv("GTAP10A_dat"),
    par_input = Sys.getenv("GTAP10A_par"),
    set_input = Sys.getenv("GTAP10A_set")
  )

  checks <- lapply(scripts, \(sc) {
    exmpl_dir <- tools::file_path_sans_ext(basename(sc))
    write_dir <- file.path(GTAPv6_dir, exmpl_dir)
    dir.create(write_dir)
    ems_option_set(tempdir = write_dir)
    source(sc, local = TRUE)
  })
  
  checks <- unlist(lapply(checks, \(r) {
    r$value
  }))
  expect_all_true(checks)
})

test_that("ems_example converted GTAPv6 scripts run without errors", {
  converted <- GTAP_convert(
    dat_har = Sys.getenv("GTAP12_dat"),
    par_har = Sys.getenv("GTAP12_par"),
    set_har = Sys.getenv("GTAP12_set"),
    target = "GTAPv6"
  )
  
  scripts <- ems_example(
    "GTAPv6",
    cvrtGTAPv6_dir,
    "scripts",
    dat_input = converted$dat,
    par_input = converted$par,
    set_input = converted$set
  )
  
  checks <- lapply(scripts, \(sc) {
    exmpl_dir <- tools::file_path_sans_ext(basename(sc))
    write_dir <- file.path(cvrtGTAPv6_dir, exmpl_dir)
    dir.create(write_dir)
    ems_option_set(tempdir = write_dir)
    source(sc, local = TRUE)
  })
  
  checks <- unlist(lapply(checks, \(r) {
    r$value
  }))
  expect_all_true(checks)
})

test_that("ems_example GTAPv7 scripts run without errors", {
  scripts <- ems_example(
    "GTAPv7",
    GTAPv7_dir,
    "scripts",
    dat_input,
    par_input,
    set_input
  )

  checks <- lapply(scripts, \(sc) {
    exmpl_dir <- tools::file_path_sans_ext(basename(sc))
    write_dir <- file.path(GTAPv7_dir, exmpl_dir)
    dir.create(write_dir)
    ems_option_set(tempdir = write_dir)
    source(sc, local = TRUE)
  })
  
  checks <- unlist(lapply(checks, \(r) {
    r$value
  }))
  expect_all_true(checks)
})

test_that("ems_example converted GTAPv7 scripts run without errors", {
  converted <- GTAP_convert(
    dat_har = Sys.getenv("GTAP10A_dat"),
    par_har = Sys.getenv("GTAP10A_par"),
    set_har = Sys.getenv("GTAP10A_set"),
    target = "GTAPv7"
  )
  
  scripts <- ems_example(
    "GTAPv7",
    cvrtGTAPv7_dir,
    "scripts",
    dat_input = converted$dat,
    par_input = converted$par,
    set_input = converted$set
  )
  
  checks <- lapply(scripts, \(sc) {
    exmpl_dir <- tools::file_path_sans_ext(basename(sc))
    write_dir <- file.path(cvrtGTAPv7_dir, exmpl_dir)
    dir.create(write_dir)
    ems_option_set(tempdir = write_dir)
    source(sc, local = TRUE)
  })
  
  checks <- unlist(lapply(checks, \(r) {
    r$value
  }))
  expect_all_true(checks)
})

test_that("ems_example GTAP-INT scripts run without errors", {
  scripts <- ems_example(
    "GTAP-INT",
    GTAP_INT_dir,
    "scripts",
    dat_input = Sys.getenv("GTAP10A_dat"),
    par_input = Sys.getenv("GTAP10A_par"),
    set_input = Sys.getenv("GTAP10A_set")
  )

  checks <- lapply(scripts, \(sc) {
    exmpl_dir <- tools::file_path_sans_ext(basename(sc))
    write_dir <- file.path(GTAP_INT_dir, exmpl_dir)
    dir.create(write_dir)
    ems_option_set(tempdir = write_dir)
    source(sc, local = TRUE)
  })

  checks <- unlist(lapply(checks, \(r) {
    r$value
  }))
  expect_all_true(checks)
})

test_that("ems_example converted GTAP-INT scripts run without errors", {
  converted <- GTAP_convert(
    dat_har = Sys.getenv("GTAP12_dat"),
    par_har = Sys.getenv("GTAP12_par"),
    set_har = Sys.getenv("GTAP12_set"),
    target = "GTAPv6"
  )

  scripts <- ems_example(
    "GTAP-INT",
    cvrtGTAP_INT_dir,
    "scripts",
    dat_input = converted$dat,
    par_input = converted$par,
    set_input = converted$set
  )

  checks <- lapply(scripts, \(sc) {
    exmpl_dir <- tools::file_path_sans_ext(basename(sc))
    write_dir <- file.path(cvrtGTAP_INT_dir, exmpl_dir)
    dir.create(write_dir)
    ems_option_set(tempdir = write_dir)
    source(sc, local = TRUE)
  })

  checks <- unlist(lapply(checks, \(r) {
    r$value
  }))
  expect_all_true(checks)
})

test_that("ems_example GTAP-RE scripts run without errors", {
  scripts <- ems_example(
    "GTAP-RE",
    GTAP_RE_dir,
    "scripts",
    dat_input,
    par_input,
    set_input
  )

  checks <- lapply(scripts, \(sc) {
    exmpl_dir <- tools::file_path_sans_ext(basename(sc))
    write_dir <- file.path(GTAP_RE_dir, exmpl_dir)
    dir.create(write_dir)
    ems_option_set(tempdir = write_dir)
    source(sc, local = TRUE)
  })

  checks <- unlist(lapply(checks, \(r) {
    r$value
  }))
  expect_all_true(checks)
})

test_that("ems_example converted GTAP-RE scripts run without errors", {
  converted <- GTAP_convert(
    dat_har = Sys.getenv("GTAP10A_dat"),
    par_har = Sys.getenv("GTAP10A_par"),
    set_har = Sys.getenv("GTAP10A_set"),
    target = "GTAPv7"
  )

  scripts <- ems_example(
    "GTAP-RE",
    cvrtGTAP_RE_dir,
    "scripts",
    dat_input = converted$dat,
    par_input = converted$par,
    set_input = converted$set
  )

  checks <- lapply(scripts, \(sc) {
    exmpl_dir <- tools::file_path_sans_ext(basename(sc))
    write_dir <- file.path(cvrtGTAP_RE_dir, exmpl_dir)
    dir.create(write_dir)
    ems_option_set(tempdir = write_dir)
    source(sc, local = TRUE)
  })

  checks <- unlist(lapply(checks, \(r) {
    r$value
  }))
  expect_all_true(checks)
})

unlink(tools::R_user_dir("teems", "cache"), recursive = TRUE)