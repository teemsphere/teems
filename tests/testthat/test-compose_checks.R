# ems_compose's consistency checks between the TAB extract and the
# solver's outputs. Most are internal assertions: a solved run never
# trips them, so each is reached here on a hand-built fixture

test_that("sets absent from the binary outputs abort by name", {
  bin_sets <- tibble::tibble(setname = "reg", ele = list(c("a", "b")))
  expect_snapshot_error(
    .check_set_consistency(
      bin_sets = bin_sets,
      tab_sets = list(REG = c("a", "b"), COMM = c("f", "m")),
      call = NULL
    )
  )
})

test_that("sets whose elements differ from the solver's are named before the abort", {
  bin_sets <- tibble::tibble(
    setname = c("reg", "comm"),
    ele = list(c("a", "b"), c("f", "x"))
  )
  tab_sets <- list(REG = c("a", "b"), COMM = c("f", "m"))
  expect_message(
    expect_error(
      .check_set_consistency(bin_sets = bin_sets, tab_sets = tab_sets, call = NULL),
      "Tablo-parsed sets/elements do not match binary set outputs",
      fixed = TRUE
    ),
    "Sets whose parsed elements differ from the solver's: \"COMM\"",
    fixed = TRUE
  )
  expect_no_error(
    .check_set_consistency(
      bin_sets = bin_sets[1, ],
      tab_sets = tab_sets["REG"],
      call = NULL
    )
  )
})

var_fixture <- function() {
  list(
    data_dt = data.table::data.table(r_idx = 1:4, Value = c(1, 2, 3, 4)),
    var_extract = tibble::tibble(
      name = "x",
      label = "test variable",
      ls_upper_idx = list(c("REG", "COMM")),
      ls_mixed_idx = list(c("REGr", "COMMc"))
    ),
    vars = tibble::tibble(cofname = "x", setid = "0,1", size = 2L, matsize = 4L),
    sets = list(REG = c("a", "b"), COMM = c("f", "m"))
  )
}

compose_var_fixture <- function(fx) {
  .compose_var(
    data_dt = fx$data_dt, var_extract = fx$var_extract, vars = fx$vars,
    sets = fx$sets, time_steps = NULL, call = NULL
  )
}

test_that("a consistent variable fixture composes", {
  expect_no_error(compose_var_fixture(var_fixture()))
})

test_that("variable values that do not fill their index space abort", {
  fx <- var_fixture()
  fx$vars$matsize <- 3L
  expect_error(compose_var_fixture(fx), "Output variable index mismatch", fixed = TRUE)
})

test_that("variable columns absent from the tab extract abort", {
  fx <- var_fixture()
  fx$var_extract$ls_upper_idx <- list(c("REG", "ACTS"))
  expect_error(compose_var_fixture(fx), "Lax column check failed", fixed = TRUE)
})

test_that("variable columns out of order against the tab extract abort", {
  fx <- var_fixture()
  fx$var_extract$ls_upper_idx <- list(c("COMM", "REG"))
  expect_error(compose_var_fixture(fx), "Strict column check failed", fixed = TRUE)
})

test_that("variable names that differ from the tab extract abort", {
  fx <- var_fixture()
  fx$var_extract$name <- "y"
  expect_snapshot_error(compose_var_fixture(fx))
})

test_that("a coefficient file that carries another coefficient aborts", {
  coeff_dir <- withr::local_tempdir()
  path <- file.path(coeff_dir, "SAVE.csv")
  writeLines(
    c('2 Real SpreadSheet Header "OTHR" LongName "some other coefficient";', "1", "2", ""),
    path
  )
  expect_error(
    .compose_coeff(
      paths = path,
      coeff_extract = tibble::tibble(name = "SAVE", ls_mixed_idx = list("REGr")),
      sets = list(REG = c("a", "b")),
      time_steps = NULL,
      call = NULL
    ),
    "coefficients absent from model output",
    fixed = TRUE
  )
})

test_that("a coefficient dimensioned on an unknown set aborts", {
  expect_snapshot_error(
    .parse_coeff_block(
      dimen = 2L, col_nmes = "REGr", num_ls = c("1", "2"),
      sets = list(COMM = c("f", "m")), call = NULL
    )
  )
})
