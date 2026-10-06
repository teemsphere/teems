# .fossil_vaen(): ELFVAEN (EFVE) after aggregation on the GTAP-E and
# GTAP-Power layers. Four raw activities in one region: coal and gas
# mining with a natural-resource rent, and two technologies without
# energy inputs that carry the database placeholder and map together.
efve_fixture <- function() {
  endw <- c("capital", "natlres")
  acts <- c("coa", "gas", "sol", "wnd")
  reg <- "r1"
  comm <- c("coa", "gas", "mfg")
  evfp <- array(0, dim = c(2L, 4L, 1L), dimnames = list(ENDW = endw, ACTS = acts, REG = reg))
  evfp["capital", , "r1"] <- c(40, 49.9, 50, 50)
  evfp["natlres", , "r1"] <- c(10, 0.1, 0, 0)
  vdfp <- array(0, dim = c(3L, 4L, 1L), dimnames = list(COMM = comm, ACTS = acts, REG = reg))
  vdfp["coa", , "r1"] <- c(10, 0, 0, 0)
  vdfp["mfg", , "r1"] <- c(40, 50, 50, 50)
  sply <- matrix(c(10, 4), ncol = 1L, dimnames = list(FOSSIL = c("coa", "gas"), REG = reg))
  efve <- data.table::data.table(ACTS = c("coal", "gas", "peak"), REG = reg, Value = c(3.8, 1.18, 1e-6))
  class(efve) <- c("EFVE", "par", class(efve))
  list(
    agg = list(EFVE = efve),
    i_data = list(EVFP = evfp, VDFP = vdfp, VMFP = vdfp * 0, SPLY = sply),
    set_raw = list(COME = c("coa", "gas"), FUEL = c("coa", "gas")),
    maps = list(
      ACTS = data.table::data.table(origin = acts, mapping = c("coal", "gas", "peak", "peak")),
      REG = data.table::data.table(origin = reg, mapping = reg)
    )
  )
}

run_fixture <- function(f, metadata = list(ep = TRUE)) {
  .fossil_vaen(
    agg_data = f$agg, i_data = f$i_data, set_raw = f$set_raw,
    set_mappings = f$maps, metadata = metadata, ndigits = 6L
  )
}

test_that("fossil mining is recalibrated to the SPLY target", {
  # coal: rent share 0.1, value-added-energy share 0.6, SPLY 10, so
  # ELFVAEN = 10 / (10 - 1/0.6) = 1.2
  out <- run_fixture(efve_fixture())
  efve <- out$data$EFVE
  expect_equal(efve[ACTS == "coal", Value], 1.2, tolerance = 1e-6)
  expect_s3_class(efve, "EFVE")
  expect_false("coal" %in% out$overrides$ACTS)
})

test_that("a recalibrated elasticity below the floor keeps the aggregated member value", {
  # gas: rent share 0.001 and SPLY 4 put the recalibration at 0.004;
  # the aggregated 1.18 stays and the override is recorded
  out <- run_fixture(efve_fixture())
  efve <- out$data$EFVE
  expect_equal(efve[ACTS == "gas", Value], 1.18)
  ov <- out$overrides[rule == "supply"]
  expect_equal(ov$ACTS, "gas")
  expect_equal(ov$from, 4 / (1000 - 2), tolerance = 1e-6)
  expect_equal(ov$to, 1.18)
})

test_that("an aggregate whose members all carry the placeholder takes 1", {
  out <- run_fixture(efve_fixture())
  efve <- out$data$EFVE
  expect_equal(efve[ACTS == "peak", Value], 1)
  ov <- out$overrides[rule == "placeholder"]
  expect_equal(ov$ACTS, "peak")
  expect_equal(ov$from, 1e-6)
  expect_equal(ov$to, 1)
  expect_equal(colnames(out$overrides), c("ACTS", "REG", "rule", "from", "to"))
})

test_that("without the layer flag only the placeholder rule applies", {
  f <- efve_fixture()
  out <- run_fixture(f, metadata = list())
  efve <- out$data$EFVE
  expect_equal(efve[ACTS %in% c("coal", "gas"), Value], c(3.8, 1.18))
  expect_equal(efve[ACTS == "peak", Value], 1)
  expect_equal(out$overrides$rule, "placeholder")
  # the input is left alone
  expect_equal(f$agg$EFVE[ACTS == "peak", Value], 1e-6)
})

test_that("a database without EFVE is passed through", {
  f <- efve_fixture()
  out <- run_fixture(list(agg = list(ESBM = f$agg$EFVE), i_data = f$i_data, set_raw = f$set_raw, maps = f$maps))
  expect_null(out$overrides)
  expect_identical(names(out$data), "ESBM")
})
