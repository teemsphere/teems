var_tbl <- tibble::tibble(
  cofname = c("pw", "lv", "lu", "q"),
  change_real = c(FALSE, TRUE, TRUE, FALSE),
  matsize = c(1L, 3L, 3L, 3L)
)
vpq <- c(pw = "price", lv = "price", lu = "price", q = "quantity")
pre <- list(lv = c(10, 20, 40), lu = c(5, 5, 5))

test_that("an exogenous change variable whose levels differ is shocked per component (R6-B4)", {
  built <- .homogeneity_shocks("lv", vpq, pre, var_tbl, "nominal")
  expect_identical(built$shocks, "Shock lv = 0.1 0.2 0.4;")
  expect_length(built$unshocked, 0L)
})

test_that("a partial entry with differing levels is reported unshocked, not dropped silently", {
  built <- .homogeneity_shocks('lv("a")', vpq, pre, var_tbl, "nominal")
  expect_length(built$shocks, 0L)
  expect_identical(built$unshocked, "lv")
})

test_that("equal levels and percentage-change variables take a uniform shock", {
  built <- .homogeneity_shocks(c("pw", "lu", "q"), vpq, pre, var_tbl, "nominal")
  expect_identical(built$shocks, c("Shock pw = uniform 1;", "Shock lu = uniform 0.05;"))
  real <- .homogeneity_shocks(c("pw", "q"), vpq, pre, var_tbl, "real")
  expect_identical(real$shocks, "Shock q = uniform 1;")
})
