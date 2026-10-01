vpq_tab <- function(...) {
  c(
    "File GTAPDATA # data #;",
    "Set REG # regions # read elements from file GTAPDATA header \"REG\";",
    ...
  )
}

test_that("VPQ types come from qualifiers, prefix defaults and Name statements", {
  tab <- vpq_tab(
    "Variable (begins p default VPQType Price)",
    "Variable (begins pf VPQType default None)",
    "Variable (begins q default VPQType Quantity)",
    "Variable (all,r,REG) pgdp(r) # price #",
    "Variable (all,r,REG) pfob(r) # foreign price #",
    "Variable (VPQType=Value)(all,r,REG) qval(r) # explicit beats default #",
    "Variable (begins q VPQType Default OFF)",
    "Variable (all,r,REG) qafter(r) # default switched off #",
    "Variable (Name x6com VPQType Quantity)",
    "Variable (all,r,REG) x6com(r) # named before its declaration #",
    "Variable (levels) Z # levels variable typed through p_Z #",
    "Variable (all,r,REG) other(r)",
    "Variable (Name other VPQType None)"
  )
  out <- .resolve_vpqtype(tab, call = NULL)
  expect_identical(
    attr(out, "vpqtype")[c("pgdp", "pfob", "qval", "qafter", "x6com", "z", "other")],
    c(pgdp = "price", pfob = "none", qval = "value", qafter = "unspecified",
      x6com = "quantity", z = "price", other = "none")
  )
  expect_false(any(grepl("begins|Name ", out)))
  expect_length(out, length(tab) - 6L)
})

test_that("an unknown VPQ type aborts", {
  expect_snapshot_error(.resolve_vpqtype(vpq_tab("Variable (begins p default VPQType Cost)"), call = NULL))
  expect_snapshot_error(.resolve_vpqtype(vpq_tab("Variable (VPQType=Cost)(all,r,REG) p(r)"), call = NULL))
})

test_that("conflicting VPQ types for one variable abort", {
  expect_snapshot_error(.resolve_vpqtype(vpq_tab(
    "Variable (VPQType=Price)(all,r,REG) p(r)",
    "Variable (Name p VPQType Value)"
  ), call = NULL))
})

test_that("a malformed VPQ type statement aborts", {
  expect_snapshot_error(.resolve_vpqtype(vpq_tab("Variable (begins p VPQType Price)"), call = NULL))
})

test_that("a Name statement for an undeclared variable warns", {
  expect_snapshot_warning(.resolve_vpqtype(vpq_tab("Variable (Name ghost VPQType Price)"), call = NULL))
})
