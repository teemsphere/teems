skip_on_cran()

test_that(".nearest_names ranks by edit distance and caps", {
  expect_identical(
    .nearest_names("qgdpp", c("qgdp", "pop", "psave")),
    "qgdp"
  )
  expect_identical(
    .nearest_names("zzzzzzzz", c("qgdp", "pop")),
    character(0)
  )
})

test_that("unknown closure variables suggest candidates", {
  var_extract <- tibble::tibble(
    name = c("qgdp", "pop", "psave"),
    condense = NA_character_
  )
  expect_snapshot_error(
    .check_closure(c("qgdpp", "pop"), var_extract, call = NULL)
  )
})

test_that("unknown closure variables without a near match omit candidates", {
  var_extract <- tibble::tibble(
    name = c("qgdp", "pop", "psave"),
    condense = NA_character_
  )
  expect_snapshot_error(
    .check_closure("zzzzzzzz", var_extract, call = NULL)
  )
})

test_that("linear names of levels variables resolve in the closure (GEMPACK manual 9.2.2)", {
  var_extract <- tibble::tibble(
    name = c("CTAXBAS", "CO2L", "qgdp"),
    qualifier_list = c("(levels,change)", "(levels)", NA),
    condense = NA_character_
  )
  closure <- .check_closure(
    c("c_CTAXBAS(REG,NEGYCOM3B)", "p_co2l", "qgdp"),
    var_extract,
    call = NULL
  )
  expect_identical(closure, c("CTAXBAS(REG,NEGYCOM3B)", "CO2L", "qgdp"))
  expect_snapshot_error(.check_closure("p_CTAXBAS", var_extract, call = NULL))
  expect_snapshot_error(.check_closure("c_qgdp", var_extract, call = NULL))
})

test_that("linear names resolve when no levels variable is a change variable", {
  var_extract <- tibble::tibble(
    name = c("INC_PC", "POP", "P", "qgdp"),
    qualifier_list = c("(levels)", "(levels)", "(levels)", NA),
    condense = NA_character_
  )
  closure <- .check_closure(
    c("p_INC_PC", "p_POP", "p_P(NFOOD,REG)", "qgdp"),
    var_extract,
    call = NULL
  )
  expect_identical(closure, c("INC_PC", "POP", "P(NFOOD,REG)", "qgdp"))
  expect_identical(
    .levels_linear_alias(c("p_inc_pc", "p_pop", "p_p", "c_"), var_extract),
    c("INC_PC", "POP", "P", "c_")
  )
})

test_that("closure names match case-insensitively and take the declared spelling", {
  var_extract <- tibble::tibble(
    name = c("delB", "xGov", "qgdp"),
    qualifier_list = NA_character_,
    condense = NA_character_
  )
  closure <- .check_closure(c("delb", "XGOV(COM)", "qgdp"), var_extract, call = NULL)
  expect_identical(closure, c("delB", "xGov(COM)", "qgdp"))
  expect_snapshot_error(.check_closure(c("delb", "Nothere"), var_extract, call = NULL))

  expect_identical(
    .canonical_entry_sets("xGov(com,\"Wool\",reg)", c("COM", "REG")),
    "xGov(COM,\"wool\",REG)"
  )
  expect_identical(.canonical_entry_sets("qgdp", c("REG")), "qgdp")
})

test_that("mixed set-index names and element literals resolve case-insensitively", {
  mixed <- c("REGr", "AGGREGA", "COMMc")
  expect_identical(.canonical_mixed(c("regr", "AGGREGA", "aggregA", "Commc", "year", "zz"), mixed),
                   c("REGr", "AGGREGA", "AGGREGA", "COMMc", "Year", "zz"))
  expect_identical(.mixed_set(c("AGGREGA", "REGr"), mixed, c("REG", "AGGREG", "COMM")[c(1, 2, 3)]),
                   c("AGGREG", "REG"))
  expect_identical(.canonical_mixed("regr", c("REGr", "REGR")), "regr")
  expect_identical(
    .canonical_entry_sets("qfd(\"Food\",acts,\"USA\")", c("ACTS", "REG")),
    "qfd(\"food\",ACTS,\"usa\")"
  )
})

square_fixture <- function(exo_rows) {
  sets <- list(ele = list(REG = c("a", "b", "c")))
  model <- tibble::tibble(
    type = "Equation",
    name = "E_x",
    tab = "Equation E_x (all,r,REG) x(r) = y(r);",
    condense_eq = NA_character_
  )
  var_extract <- tibble::tibble(
    name = c("x", "y"),
    ls_upper_idx = list(x = "REG", y = "REG")
  )
  ele <- data.table::CJ(REGr = c("a", "b", "c")[seq_len(exo_rows)])
  entry <- structure("y", var_name = "y", ele = ele)
  attr(entry, "var_name") <- "y"
  closure <- list(entry)
  size_metadata <- list(
    n_var_ele = 6,
    n_exo_ele = exo_rows
  )
  list(
    model = model, var_extract = var_extract, sets = sets,
    closure = closure, size_metadata = size_metadata
  )
}

test_that("a squared closure passes the count check", {
  fx <- square_fixture(exo_rows = 3L)
  expect_no_error(
    .check_system_square(
      model = fx$model, var_extract = fx$var_extract, sets = fx$sets,
      closure = fx$closure, size_metadata = fx$size_metadata, call = NULL
    )
  )
})

test_that("unsquared closures abort with arithmetic and candidates", {
  fx <- square_fixture(exo_rows = 2L)
  expect_snapshot_error(
    .check_system_square(
      model = fx$model, var_extract = fx$var_extract, sets = fx$sets,
      closure = fx$closure, size_metadata = fx$size_metadata, call = NULL
    )
  )
})

test_that("over-exogenized closures name endogenizing candidates", {
  fx <- square_fixture(exo_rows = 3L)
  fx$size_metadata$n_exo_ele <- 4
  expect_snapshot_error(
    .check_system_square(
      model = fx$model, var_extract = fx$var_extract, sets = fx$sets,
      closure = fx$closure, size_metadata = fx$size_metadata, call = NULL
    )
  )
})

test_that("an over-exogenized count with nothing exogenous to release names no candidate", {
  fx <- square_fixture(exo_rows = 3L)
  fx$closure <- list()
  fx$size_metadata$n_exo_ele <- 4
  expect_snapshot_error(
    .check_system_square(
      model = fx$model, var_extract = fx$var_extract, sets = fx$sets,
      closure = fx$closure, size_metadata = fx$size_metadata, call = NULL
    )
  )
})

test_that("unresolvable quantifier sets warn and skip the count check", {
  fx <- square_fixture(exo_rows = 2L)
  fx$model$tab <- "Equation E_x (all,z,MYSTERY) x(z) = 1;"
  expect_snapshot_warning(
    .check_system_square(
      model = fx$model, var_extract = fx$var_extract, sets = fx$sets,
      closure = fx$closure, size_metadata = fx$size_metadata, call = NULL
    )
  )
})

test_that("a closure entry with the wrong number of arguments aborts by name (R4-B3)", {
  var_extract <- tibble::tibble(
    name = c("tfd", "pop", "pfactwld"),
    condense = NA_character_,
    ls_upper_idx = list(c("COMM", "ACTS", "REG"), "REG", NA_character_)
  )
  expect_snapshot_error(.check_closure(c("tfd(COMM,REG)", "pop"), var_extract, call = NULL))
  expect_identical(
    .check_closure(c("tfd(COMM,ACTS,REG)", "pop", "tfd(COMM,\"food\",\"usa\")", "pfactwld"), var_extract, call = NULL),
    c("tfd(COMM,ACTS,REG)", "pop", "tfd(COMM,\"food\",\"usa\")", "pfactwld")
  )
})

test_that("unsquare-closure candidates rank the swapped-out variables first (X1-B4)", {
  var_extract <- tibble::tibble(
    name = c("psave", "qsave", "afreg", "pfactwld"),
    ls_upper_idx = list("REG", "REG", "REG", NA_character_)
  )
  sets <- list(ele = list(REG = c("a", "b", "c")))
  default <- .square_candidates(3, var_extract, sets, list())
  preferred <- .square_candidates(3, var_extract, sets, list(), prefer = "afreg")
  expect_match(default, "psave, qsave, afreg")
  expect_match(preferred, "afreg, psave, qsave")
})
