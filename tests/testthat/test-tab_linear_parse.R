skip_on_cran()

# the condensation parser: an equation side is read into linear terms
# (variable factors with their coefficient factors and sum quantifiers)
# and serialized back for the rewritten equation

test_that("a conditional sum carries its condition through parse, rename and serialize", {
  vl <- list(gco2t = "gco2t")
  side <- "sum{r,REG: REGTOBLOC(r) = b, CO2T(r)*gco2t(r)}"
  terms <- .parse_linear_side(side, vl)
  expect_length(terms, 1L)
  q <- terms[[1]]$quants[[1]]
  expect_equal(q$idx, "r")
  expect_equal(q$set, "REG")
  expect_equal(q$cond, ": REGTOBLOC(r)=b")
  expect_match(.serialize_linear(terms), "sum{r,REG: REGTOBLOC(r)=b, ", fixed = TRUE)
  expect_match(.serialize_linear(terms), "gco2t(r)}", fixed = TRUE)

  # renaming the index reaches the condition too
  renamed <- .rename_term(terms[[1]], c(r = "rr"))
  expect_equal(renamed$quants[[1]]$idx, "rr")
  expect_equal(renamed$quants[[1]]$cond, ": REGTOBLOC(rr)=b")
  expect_match(.serialize_linear(list(renamed)), "sum{rr,REG: REGTOBLOC(rr)=b, ", fixed = TRUE)
  expect_match(.serialize_linear(list(renamed)), "gco2t(rr)}", fixed = TRUE)

  # a conditional sum with no variable inside is one coefficient factor
  coef <- .parse_linear_side("sum{r,REG: REGTOBLOC(r) = b, CO2T(r)} * gco2tb(b)", list(gco2tb = "gco2tb"))
  expect_length(coef, 1L)
  expect_match(.serialize_linear(coef), "sum{r,REG: REGTOBLOC(r)=b, CO2T(r)}", fixed = TRUE)

  # commas inside the condition's references stay put; an unconditional
  # sum serializes as before
  nested <- .parse_linear_side("sum{c,COMM: MAPC(c,r) = cc, VXW(c,r)*qxw(c,r)}", list(qxw = "qxw"))
  expect_equal(nested[[1]]$quants[[1]]$cond, ": MAPC(c,r)=cc")
  plain <- .parse_linear_side("sum{r,REG, GDP(r)*pop(r)}", list(pop = "pop"))
  expect_equal(plain[[1]]$quants[[1]]$cond, "")
  expect_match(.serialize_linear(plain), "sum{r,REG, GDP(r)*pop(r)}", fixed = TRUE)

  # malformed conditions are refused, not silently dropped
  expect_error(.parse_linear_side("sum{r,REG: , CO2T(r)*gco2t(r)}", vl), "empty sum condition")
  expect_error(.parse_linear_side("sum{r,REG: REGTOBLOC(r) = b}", vl), "unterminated sum condition")
})

test_that("a reference written with square brackets keeps them", {
  vl <- list(qxw = "qxw")
  terms <- .parse_linear_side("ID01[VXW(c,r)] * qxw(c,r)", vl)
  expect_length(terms, 1L)
  expect_match(.serialize_linear(terms), "ID01[VXW(c,r)]", fixed = TRUE)
  expect_match(.serialize_linear(terms), "qxw(c,r)", fixed = TRUE)
  round <- .parse_linear_side("ID01(VXW(c,r)) * qxw(c,r)", vl)
  expect_match(.serialize_linear(round), "ID01(VXW(c,r))", fixed = TRUE)
})

# every refusal reaches the user through condense_parse's {parse_reason},
# so each reason is pinned to the input that raises it
test_that("the linear parser names each form it refuses", {
  vl <- list(x = "x", y = "y")
  refuse <- function(side, reason) {
    expect_error(.parse_linear_side(side, vl), reason, fixed = TRUE)
  }
  refuse("x(r)*y(r)", "product of two variable-bearing expressions (nonlinear)")
  refuse("A(r)/x(r)", "division by a variable-bearing expression (nonlinear)")
  refuse("sum{r,REG A(r)*x(r)}", "expected `,` but found `A`")
  refuse("x(r", "unbalanced parentheses in a reference")
  refuse("x(r) y(r)", "trailing tokens starting at `y`")
  refuse("A(r)*", "unexpected end of expression")
  refuse("* x(r)", "unexpected token `*`")
  refuse("sum{1,REG, x(r)}", "malformed sum index")
  refuse("sum{r,, x(r)}", "malformed sum set")
  refuse("ABS(x(r))", "variable reference inside the arguments of `ABS`")
  refuse("x(r) @ y(r)", "unrecognized characters {@}")
})

test_that("a sum written with square brackets is a sum (GTAP PWLDUSE)", {
  vl <- list(pp = "pp", pg = "pg", pf = "pf")
  side <- paste(
    "sum{s,REG, VPA(i,s) * pp(i,s) + VGA(i,s) * pg(i,s)",
    "+ sum[j,PROD_COMM, VFA(i,j,s) * pf(i,j,s)]}"
  )
  terms <- .parse_linear_side(side, vl)
  expect_length(terms, 3L)
  inner <- terms[[3]]$quants
  expect_length(inner, 2L)
  expect_equal(inner[[2]]$idx, "j")
  expect_equal(inner[[2]]$set, "PROD_COMM")
  expect_match(.serialize_linear(terms[3]), "sum{s,REG, sum{j,PROD_COMM, VFA(i,j,s)*pf(i,j,s)}}", fixed = TRUE)
  coef <- .parse_linear_side("sum[j,PROD_COMM, VFA(i,j,s)] * pp(i,s)", vl)
  expect_match(.serialize_linear(coef), "sum{j,PROD_COMM, VFA(i,j,s)}", fixed = TRUE)
})

test_that("a variable divided by a coefficient keeps the division when serialized", {
  vl <- list(x = "x", y = "y")
  terms <- .parse_linear_side("x(r)/C(r) + A(r)*y(r)/B(r)", vl)
  expect_equal(.serialize_linear(terms), "1/C(r)*x(r) + A(r)/B(r)*y(r)")
  expect_equal(.fac_text(c("P", "Q"), c("/", "*")), "1/P*Q")
  expect_equal(.fac_text(c("P", "Q"), c("*", "/")), "P/Q")
  expect_equal(.fac_text(character(), character()), "")
})

test_that("powers, braces, IF terms and word conditions parse (MEL43AUX E_ta, manual 11.4.6)", {
  vl <- list(d_rev = "d_rev", p_tsd = "p_tsd")
  # a coefficient raised to a power inside braces
  terms <- .parse_linear_side("C_REV(c)/{[C_PT(c) - C_REV(c)]^2}*[C_PT(c)*p_tsd(c) - 100*d_rev(c)]", vl)
  expect_length(terms, 2L)
  expect_match(.serialize_linear(terms), "]^2", fixed = TRUE)
  expect_error(.parse_linear_side("[p_tsd(c)]^2", vl), "power of a variable-bearing expression")
  # an IF around variable terms multiplies each term by IF[cond, 1]
  iff <- .parse_linear_side("IF[C_PT(c) > 0 and C_REV(c) > 0, 2*p_tsd(c) + d_rev(c)]", vl)
  expect_length(iff, 2L)
  expect_match(.serialize_linear(iff), "IF[C_PT(c)>0 and C_REV(c)>0, 1]", fixed = TRUE)
  # an IF around a coefficient stays one factor
  coef <- .parse_linear_side("IF[C_PT(c) > 0, C_REV(c)] * p_tsd(c)", vl)
  expect_length(coef, 1L)
  expect_match(.serialize_linear(coef), "IF[C_PT(c)>0, C_REV(c)]", fixed = TRUE)
  expect_error(.parse_linear_side("IF[p_tsd(c) > 0, d_rev(c)]", vl), "variable reference inside an IF condition")
  # word operators in a sum condition keep their blanks
  wc <- .parse_linear_side("sum{s,REG: s <> r and s <> \"usa\", X(s)*p_tsd(s)}", vl)
  expect_equal(wc[[1]]$quants[[1]]$cond, ": s<>r and s<>\"usa\"")
})
