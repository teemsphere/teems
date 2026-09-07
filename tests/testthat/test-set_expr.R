skip_on_cran()

test_that("set difference removes aggregated elements entirely", {
  # two origins map to the aggregated margin commodity; subtracting the
  # margin set must drop the element itself, not just the shared rows
  # (GEMPACK manual 10.1.1.1)
  mappings <- list(
    COMM = data.table::data.table(
      origin = c("o1", "o2", "o3", "o4"),
      mapping = c("crops", "mnfcs", "svces", "svces"),
      key = c("origin", "mapping")
    ),
    MARG = data.table::data.table(
      origin = "o3",
      mapping = "svces",
      key = c("origin", "mapping")
    )
  )
  out <- .eval_set_expr(
    d = "= COMM - MARG",
    mappings = mappings,
    owner = "NMRG",
    call = NULL
  )
  expect_identical(unique(out$mapping), c("crops", "mnfcs"))
})

test_that("set difference rejects elements absent from the minuend", {
  mappings <- list(
    A = data.table::data.table(
      origin = "o1", mapping = "x",
      key = c("origin", "mapping")
    ),
    B = data.table::data.table(
      origin = "o2", mapping = "y",
      key = c("origin", "mapping")
    )
  )
  expect_error(
    .eval_set_expr(d = "= A - B", mappings = mappings, owner = "C", call = NULL),
    "may only remove elements that are present"
  )
})

test_that("set union rejects element-level overlap with disjoint origins", {
  # the same aggregated element carried by different origin rows used
  # to slip past the row-level disjointness test
  mappings <- list(
    A = data.table::data.table(
      origin = "o1", mapping = "x",
      key = c("origin", "mapping")
    ),
    B = data.table::data.table(
      origin = "o2", mapping = "x",
      key = c("origin", "mapping")
    )
  )
  expect_error(
    .eval_set_expr(d = "= A + B", mappings = mappings, owner = "C", call = NULL),
    "requires disjoint sets"
  )
})

test_that("set intersection is element-level with agreeing origins", {
  mappings <- list(
    A = data.table::data.table(
      origin = c("o1", "o2"), mapping = c("x", "y"),
      key = c("origin", "mapping")
    ),
    B = data.table::data.table(
      origin = c("o1", "o3"), mapping = c("x", "z"),
      key = c("origin", "mapping")
    )
  )
  out <- .eval_set_expr(
    d = "= A & B",
    mappings = mappings,
    owner = "C",
    call = NULL
  )
  expect_identical(unique(out$mapping), "x")
  expect_identical(out$origin, "o1")
})

test_that("set intersection keeps shared elements and stamps disagreeing origins", {
  # both operands contain element x, but via different origins: the
  # old row-level fintersect silently DROPPED x, and an intermediate
  # revision aborted. Element-level semantics (manual 10.1.1) keep x
  # with the accumulator's rows; the disagreement is recorded as the
  # origin_conflict stamp for consumers that read origin rows
  # (.finalize_map_data)
  mappings <- list(
    A = data.table::data.table(
      origin = "o1", mapping = "x",
      key = c("origin", "mapping")
    ),
    B = data.table::data.table(
      origin = "o2", mapping = "x",
      key = c("origin", "mapping")
    )
  )
  out <- .eval_set_expr(d = "= A & B", mappings = mappings, owner = "C", call = NULL)
  expect_identical(unique(out$mapping), "x")
  expect_identical(out$origin, "o1")
  expect_identical(attr(out, "origin_conflict"), "x")
})

test_that("the origin_conflict stamp propagates through derived sets", {
  mappings <- list(
    A = data.table::data.table(
      origin = "o1", mapping = "x",
      key = c("origin", "mapping")
    ),
    B = data.table::data.table(
      origin = "o2", mapping = "x",
      key = c("origin", "mapping")
    ),
    E = data.table::data.table(
      origin = "o9", mapping = "y",
      key = c("origin", "mapping")
    )
  )
  tainted <- .eval_set_expr(d = "= A & B", mappings = mappings, owner = "C", call = NULL)
  mappings$C <- tainted
  # the conflicted element survives into the union: stamp carries
  derived <- .eval_set_expr(d = "= C + E", mappings = mappings, owner = "D", call = NULL)
  expect_identical(attr(derived, "origin_conflict"), "x")
  # the conflicted element is subtracted away: stamp trimmed off
  cleaned <- .eval_set_expr(d = "= C - B", mappings = mappings, owner = "F", call = NULL)
  expect_null(attr(cleaned, "origin_conflict"))
})

test_that("set products name elements per GEMPACK 11.7.11, first factor fastest", {
  dt <- function(...) {
    x <- c(...)
    data.table::data.table(origin = x, mapping = x, key = c("origin", "mapping"))
  }
  mappings <- list(
    UNITC = dt("c"),
    COMM = dt("crops", "food", "livestock", "mnfcs", "svces"),
    REG = dt("chn", "row", "usa"),
    LONGC = dt("agriculture", "manufacturing", "services"),
    LONGR = dt("northamerica", "europeanunion")
  )
  ev <- function(d, owner) {
    .eval_set_expr(d = d, mappings = mappings, owner = owner, call = NULL)$mapping
  }
  # short names: xx_yyy, both spellings of the operator
  expect_identical(
    ev("= UNITC x COMM", "UCOM"),
    c("c_crops", "c_food", "c_livestock", "c_mnfcs", "c_svces")
  )
  expect_identical(ev("= UNITC X COMM", "UCOM"), ev("= UNITC x COMM", "UCOM"))
  # livestock(9) + chn(3) > 11: the long factor truncates to 11 - 3 = 8,
  # "<set letter><number><leading chars>"; first factor varies fastest
  cr <- ev("= COMM x REG", "CR")
  expect_length(cr, 15L)
  expect_identical(cr[1:6], c(
    "c1crops_chn", "c2food_chn", "c3livest_chn", "c4mnfcs_chn", "c5svces_chn",
    "c1crops_row"
  ))
  # one long factor against a short one
  expect_identical(
    ev("= LONGC x REG", "LT")[1:3],
    c("l1agricu_chn", "l2manufa_chn", "l3servic_chn")
  )
  # both long: 6 and 5 characters; the element numbers follow the order
  # the tables hold (keyed tables sort explicit elements: europeanunion
  # before northamerica), which is also the order deployed to the solver
  expect_identical(
    ev("= LONGC x LONGR", "LL"),
    c("l1agri_l1eur", "l2manu_l1eur", "l3serv_l1eur",
      "l1agri_l2nor", "l2manu_l2nor", "l3serv_l2nor")
  )
  # products compose with the other operators (ALLOCEFF shape)
  expect_identical(
    ev("= REG + (UNITC x COMM)", "ALLOCEFF"),
    c("chn", "row", "usa", "c_crops", "c_food", "c_livestock", "c_mnfcs", "c_svces")
  )
  # implied-subset census: a product implies no subset relation
  info <- .set_expr_info("= UNITC x COMM")
  expect_false(info$all_plus_union)
  expect_identical(info$ops, "*")
})

test_that("set products reject duplicate element names", {
  dt <- function(...) {
    x <- c(...)
    data.table::data.table(origin = x, mapping = x, key = c("origin", "mapping"))
  }
  mappings <- list(DA = dt("ab_c", "ab"), DB = dt("d", "c_d"))
  expect_error(
    .eval_set_expr(d = "= DA x DB", mappings = mappings, owner = "DP", call = NULL),
    "duplicate element name"
  )
})
