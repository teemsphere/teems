mk <- function(values, idx) {
  dt <- data.table::data.table(idx, Value = values)
  data.table::setkeyv(dt, setdiff(names(dt), "Value"))
  dt
}

test_that("a zero in one member of a valuation chain zeroes the cell in every member and its margins", {
  withr::defer(ems_option_reset())
  idx <- data.table::CJ(COMM = c("food", "mnfcs"), REG = c("chn", "usa"), REG.1 = c("chn", "usa"))
  vxsb <- mk(c(0, 5, 7, 9, 11, 13, 15, 17), idx)
  vfob <- mk(c(2e-6, 5.5, 7.5, 9.5, 11.5, 13.5, 15.5, 17.5), idx)
  vcif <- mk(c(3e-6, 6, 8, 10, 12, 0, 16, 18), idx)
  vmsb <- mk(c(3e-6, 6, 8, 10, 12, 14, 16, 18), idx)
  vtwr <- mk(rep(0.5, 16), data.table::CJ(MARG = c("otp", "wtp"), COMM = c("food", "mnfcs"), REG = c("chn", "usa"), REG.1 = c("chn", "usa")))
  evfb <- mk(c(1, 2), data.table::CJ(ENDW = c("labor", "capital")))
  .data <- list(VXSB = vxsb, VFOB = vfob, VCIF = vcif, VMSB = vmsb, VTWR = vtwr, EVFB = evfb)
  out <- .zero_families(.data, 1e-6)
  zeroed <- out$VXSB$Value == 0
  expect_identical(which(zeroed), c(1L, 6L))
  for (h in c("VFOB", "VCIF", "VMSB")) expect_identical(out[[h]]$Value == 0, zeroed, label = h)
  expect_identical(out$VFOB$Value[2], 5.5)
  margins <- out$VTWR[out$VXSB[zeroed, c("COMM", "REG", "REG.1")], on = c("COMM", "REG", "REG.1")]
  expect_true(all(margins$Value == 0))
  expect_identical(sum(out$VTWR$Value == 0), 4L)
  expect_identical(out$EVFB$Value, c(1, 2))
})

test_that("the chain step is a no-op when the floor is off or a member is missing", {
  idx <- data.table::CJ(COMM = c("food", "mnfcs"), REG = c("chn", "usa"))
  vdfb <- mk(c(0, 1, 2, 3), idx)
  vdfp <- mk(c(0.1, 1.1, 2.1, 3.1), idx)
  same <- .zero_families(list(VDFB = data.table::copy(vdfb), VDFP = data.table::copy(vdfp)), 0)
  expect_identical(same$VDFP$Value, c(0.1, 1.1, 2.1, 3.1))
  alone <- .zero_families(list(VDFB = data.table::copy(vdfb)), 1e-6)
  expect_identical(alone$VDFB$Value, c(0, 1, 2, 3))
  both <- .zero_families(list(VDFB = data.table::copy(vdfb), VDFP = data.table::copy(vdfp)), 1e-6)
  expect_identical(both$VDFP$Value, c(0, 1.1, 2.1, 3.1))
})
