test_that("a weight entry is read as its header, sign and set restriction", {
  expect_equal(
    .weight_entry("VDFP"),
    list(header = "VDFP", flip = FALSE, dim = NA_character_, set = NA_character_)
  )
  expect_true(.weight_entry("-FBEP")$flip)
  expect_equal(
    .weight_entry("VDFP[COMM=COME]"),
    list(header = "VDFP", flip = FALSE, dim = "COMM", set = "COME")
  )
})

test_that("a weight entry restricted to a set keeps only its elements", {
  arr <- array(
    1:12,
    dim = c(3L, 2L, 2L),
    dimnames = list(COMM = c("COA", "Gas", "crops"), ACTS = c("a1", "a2"), REG = c("r1", "r2"))
  )
  entry <- .weight_entry("VDFP[COMM=COME]")
  sets <- list(COME = c("coa", "gas"))
  kept <- .weight_restrict(weight = arr, entry = entry, sets = sets)
  expect_equal(dimnames(kept)$COMM, c("COA", "Gas"))
  expect_equal(sum(kept), sum(arr[1:2, , ]))
  dt <- data.table::data.table(COMM = c("coa", "gas", "crops"), Value = 1:3)
  expect_equal(.weight_restrict(weight = dt, entry = entry, sets = sets)$Value, 1:2)
  # a set or dimension the database lacks is a named abort, not an
  # empty weight that would silently default the parameter
  expect_snapshot_error(.weight_restrict(weight = arr, entry = entry, sets = list()))
  expect_snapshot_error(.weight_restrict(weight = dt[, .(Value)], entry = entry, sets = sets))
})

test_that("a parameter whose aggregate has no weight is the mean of its members", {
  # an activity absent from a region has no output to weight ETRQ by;
  # a zero would fail the model's ETRAQ lt 0 assertion
  dt <- data.table::data.table(
    ACTS = c("a1", "a2", "a3"),
    Value = c(-5, -3, 4),
    omega = c(0, 0, 1),
    sigma = c(0, 0, 4)
  )
  class(dt) <- c("ETRQ", "par", class(dt))
  sets <- list(ACTS = data.table::data.table(origin = c("a1", "a2", "a3"), mapping = c("z", "z", "y")))
  out <- .aggregate_data.par(dt, sets = sets, ndigits = 6L)
  expect_equal(out[ACTS == "z", Value], -4)
  expect_equal(out[ACTS == "y", Value], 4)
  expect_equal(colnames(out), c("ACTS", "Value"))
  # the share weight falls back to the value weight before the mean
  dt <- data.table::data.table(
    ACTS = c("a1", "a2"),
    Value = c(2, 6),
    omega = c(0, 0),
    sigma = c(0, 0),
    omega_v = c(3, 1),
    sigma_v = c(6, 6)
  )
  class(dt) <- c("ESBM", "par", class(dt))
  sets <- list(ACTS = data.table::data.table(origin = c("a1", "a2"), mapping = c("z", "z")))
  out <- .aggregate_data.par(dt, sets = sets, ndigits = 6L)
  expect_equal(out$Value, 3)
})

test_that("a region-generic parameter is world-weighted under the value method", {
  # ESBD a: 2 in r1, 4 in r2; b: 6 everywhere. Household purchases a:
  # 1 in r1, 3 in r2; b: 1 in each; nothing else. FlexAgg's world
  # weights are a 4, b 2, and a's world mean is 3.5, so the aggregate
  # c = (3.5 * 4 + 6 * 2) / 6 in both regions. The share method keeps
  # the region's own weights (every nest share is 1, so it falls back
  # to the region's value weights: r1 (2 + 6) / 2, r2 (4 * 3 + 6) / 4)
  comm <- c("a", "b")
  reg <- c("r1", "r2")
  esbd <- data.table::data.table(
    COMM = rep(comm, each = 2), REG = rep(reg, 2), Value = c(2, 4, 6, 6)
  )
  class(esbd) <- c("ESBD", "par", class(esbd))
  mk <- function(v) array(v, dim = c(2L, 2L), dimnames = list(COMM = comm, REG = reg))
  weights <- list(VDPP = mk(c(1, 1, 3, 1)))
  for (h in c("VMPP", "VMGP", "VDGP", "VDFP", "VMFP", "VDIP", "VMIP")) {
    weights[[h]] <- mk(0)
  }
  maps <- list(
    COMM = data.table::data.table(origin = comm, mapping = c("c", "c")),
    REG = data.table::data.table(origin = reg, mapping = reg)
  )
  agg <- function(method) {
    w <- .weight_param(
      i_data = list(ESBD = data.table::copy(esbd)), weights = weights, sets = list(),
      set_mappings = maps, methods = c(ESBD = method), data_format = "GTAPv7"
    )
    .aggregate_data.par(w$ESBD, sets = maps, ndigits = 6L)
  }
  expect_equal(agg("value")$Value, rep((3.5 * 4 + 6 * 2) / 6, 2), tolerance = 1e-6)
  expect_equal(agg("share")$Value, c(4, 4.5))
})
