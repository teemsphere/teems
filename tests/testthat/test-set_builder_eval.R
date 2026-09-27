# conditional set builders evaluated at deploy (GEMPACK manual 10.1.2):
# the R mirror of the solver's tab_setbuilder_transform, on a two-by-two
# coefficient so every refusal is reached without a database

sb_fixture <- function() {
  ele_map <- function(x) {
    data.table::data.table(origin = x, mapping = x, key = c("origin", "mapping"))
  }
  list(
    mappings = list(COMM = ele_map(c("food", "mnfcs")), REG = ele_map(c("chn", "usa"))),
    coeff_extract = tibble::tibble(
      name = c("VDFB", "NODT"),
      header = c("VDFB", "NODT"),
      qualifier_list = NA_character_
    ),
    coeff_data = list(VDFB = data.table::data.table(
      COMM = c("food", "mnfcs", "food", "mnfcs"),
      REG = c("chn", "chn", "usa", "usa"),
      Value = c(1, 0, 2, 0)
    ))
  )
}

eval_builder <- function(definition, fx = sb_fixture()) {
  .eval_set_builder(
    b = .parse_set_builder(definition),
    owner = "NEWSET",
    mappings = fx$mappings,
    coeff_data = fx$coeff_data,
    coeff_extract = fx$coeff_extract,
    call = NULL
  )
}

test_that("a builder keeps the source elements its condition selects", {
  out <- eval_builder('= (all,c,COMM: VDFB(c,"chn") > 0)')
  expect_identical(out$mapping, "food")
  out <- eval_builder('= (all,c,COMM: VDFB(c,"usa") = 0)')
  expect_identical(out$mapping, "mnfcs")
})

test_that("a builder waits for a source set that is not resolved yet", {
  expect_null(eval_builder('= (all,a,ACTS: VDFB(a,"chn") > 0)'))
})

test_that("a builder whose coefficient has no loaded data aborts", {
  expect_snapshot_error(eval_builder('= (all,c,COMM: NODT(c,"chn") > 0)'))
})

test_that("a builder with the wrong number of arguments aborts", {
  expect_snapshot_error(eval_builder("= (all,c,COMM: VDFB(c) > 0)"))
})

test_that("a builder naming an element outside the aggregation aborts", {
  expect_snapshot_error(eval_builder('= (all,c,COMM: VDFB(c,"fra") > 0)'))
})

test_that("a builder looping over the wrong dimension aborts", {
  expect_snapshot_error(eval_builder('= (all,c,COMM: VDFB("food",c) > 0)'))
})

test_that("a builder that selects nothing yields an empty set", {
  ems_option_set(verbose = FALSE)
  withr::defer(ems_option_reset())
  out <- eval_builder('= (all,c,COMM: VDFB(c,"chn") > 5)')
  expect_identical(nrow(out), 0L)
  ems_option_set(verbose = TRUE)
  expect_snapshot(out <- eval_builder('= (all,c,COMM: VDFB(c,"chn") > 5)'))
})

test_that("the set-condition evaluator reads the statistical functions and TRUNCB (manual 11.5.3-11.5.7)", {
  ev <- \(e) .sbx_eval(.sbx_parse(e), list(), list())$v
  expect_equal(ev("NORMAL(1)"), 0.241970725, tolerance = 1e-8)
  expect_equal(ev("CUMNORMAL[2]"), 0.977249868, tolerance = 1e-8)
  expect_equal(ev("LOGNORMAL{2}"), 0.156874019, tolerance = 1e-8)
  expect_equal(ev("CUMLOGNORMAL(2)"), 0.755891404, tolerance = 1e-8)
  expect_identical(ev("LOGNORMAL(0)"), 0)
  expect_identical(ev("CUMLOGNORMAL(0-1)"), 0)
  expect_equal(ev("GPERF(0-1)"), -0.842700793, tolerance = 1e-8)
  expect_equal(ev("GPERFC(1)"), 0.157299207, tolerance = 1e-8)
  expect_equal(ev("CUMNORMAL(0.7) - 0.5*(1 + GPERF(0.7/SQRT(2)))"), 0, tolerance = 1e-12)
  expect_identical(ev("TRUNCB(0-0.5)"), -1)
})

test_that("a mapping-sum builder without its mapping aborts", {
  expect_snapshot_error(
    eval_builder("= (all,c,COMM: sum{r,REG: MAPRC(r) = c, VDFR(r)} > 0)")
  )
})

formula_builder_fixture <- function() {
  model <- .process_tablo(test_path("fixtures", "builders", "formula_builders.tab"), call = NULL)
  ele_map <- function(x) data.table::data.table(origin = x, mapping = x)
  make <- data.table::CJ(COM = c("c1", "c2", "c3"), IND = c("i1", "i2", "i3"), REG = c("r1", "r2"), sorted = FALSE)
  make$Value <- 0
  make[COM == "c1" & IND == "i1", Value := 3]
  make[COM == "c2" & IND == "i1", Value := 1]
  make[COM == "c2" & IND == "i2", Value := 2]
  make[COM == "c3" & IND == "i2", Value := 2]
  make[COM == "c3" & IND == "i3", Value := 1e-7]
  class(make) <- c("MAKE", "dat", class(make))
  list(
    model = model,
    mappings = list(COM = ele_map(c("c1", "c2", "c3")), IND = ele_map(c("i1", "i2", "i3")), REG = ele_map(c("r1", "r2"))),
    coeff_data = list(MAKE = make),
    set_raw = list(I2R = c("r1", "r2", "r1"))
  )
}

eval_formula_builders <- function(fx) {
  model <- fx$model
  mappings <- fx$mappings
  for (r in which(model$type == "Set")) {
    d <- model$definition[[r]]
    if (!.is_set_builder(d)) {
      next
    }
    mappings[[model$name[r]]] <- .eval_set_builder(
      b = .parse_set_builder(d), owner = model$name[r], mappings = mappings,
      coeff_data = fx$coeff_data, coeff_extract = model[model$type == "Coefficient", ],
      call = NULL, model = model, set_raw = fx$set_raw
    )
  }
  mappings
}

test_that("builders over Formula coefficients are evaluated from the Formula chain", {
  ems_option_set(verbose = FALSE)
  withr::defer(ems_option_reset())
  fx <- formula_builder_fixture()
  sb <- attr(fx$model, "set_builders")
  expect_identical(unname(purrr::map_chr(sb, "set")), c("MIND", "MINDCOM", "LOCIND", "FIRST", "BOTH", "R1IND"))
  expect_true(any(grepl("Set MIND # multi-product industries # = (all,i,IND: SBI01(i) > 0.5);", fx$model$tab, fixed = TRUE)))
  maps <- eval_formula_builders(fx)
  ele <- \(s) maps[[s]]$mapping
  expect_identical(ele("MIND"), c("i1", "i2"))
  expect_identical(ele("MINDCOM"), c("c1", "c2", "c3"))
  expect_identical(ele("LOCIND"), character(0))
  expect_identical(ele("FIRST"), "i1")
  expect_identical(ele("BOTH"), "i1")
  expect_identical(ele("R1IND"), c("i1", "i3"))
  ind <- attr(maps[["MIND"]], "sb_indicator")
  expect_identical(class(ind)[1], "SB01")
  expect_identical(names(ind), c("IND", "Value"))
  expect_identical(ind$Value, c(1, 1, 0))
})

test_that("a builder over a formula waits for the sets its chain needs", {
  fx <- formula_builder_fixture()
  spec <- attr(fx$model, "set_builders")[["SBI02"]]
  out <- .eval_set_builder_formula(
    spec = spec, owner = "MINDCOM", src_map = fx$mappings$COM, mappings = fx$mappings,
    coeff_data = fx$coeff_data, model = fx$model, set_raw = fx$set_raw, call = NULL
  )
  expect_null(out)
})

test_that("a formula builder that cannot be evaluated names the reason", {
  fx <- formula_builder_fixture()
  fx$coeff_data <- list()
  expect_snapshot_error(eval_formula_builders(fx))
})

test_that("the set-condition evaluator reads sums, IF, functions and $POS", {
  expect_null(tryCatch(.sbx_parse("sum{i,IND:"), error = \(e) NULL))
  node <- .sbx_parse('if[x(i) > 1 and not y(i) = "a", ABS[-2]^2] + $pos(i,IND)')
  expect_identical(node$t, "bin")
  expect_setequal(.sbx_names(node, "i"), c("x", "y", "IND"))
  expect_setequal(.sbx_names(.sbx_parse("sum{j,S:m(j)=i, c(j)}"), "i"), c("S", "m", "c"))
})
