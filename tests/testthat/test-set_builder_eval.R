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

test_that("a builder that selects nothing aborts", {
  expect_snapshot_error(eval_builder('= (all,c,COMM: VDFB(c,"chn") > 5)'))
})

test_that("a mapping-sum builder without its mapping aborts", {
  expect_snapshot_error(
    eval_builder("= (all,c,COMM: sum{r,REG: MAPRC(r) = c, VDFR(r)} > 0)")
  )
})
