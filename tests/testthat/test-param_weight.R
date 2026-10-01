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
