time_steps <- data.table::data.table(all_time = 0:2, CYRS = c(2017, 2018, 2020))
int_set <- function() structure(character(0), intertemporal = TRUE)

test_that("one intertemporal dimension gets a Year column", {
  sets <- list(ALLTIME = int_set(), REG = character(0))
  dt <- data.table::data.table(REGr = c("a", "b"), ALLTIMEt = c(0, 2), Value = 1:2)
  out <- .match_year(dt, sets, time_steps)
  expect_identical(out$Year, c(2017, 2020))
})

test_that("each intertemporal dimension gets its own Year column", {
  sets <- list(ALLTIME = int_set(), FWDTIME = int_set())
  dt <- data.table::data.table(ALLTIMEt = c(0, 1), FWDTIMEs = c(1, 2), Value = 1:2)
  out <- .match_year(dt, sets, time_steps)
  expect_identical(out$Year_ALLTIMEt, c(2017, 2018))
  expect_identical(out$Year_FWDTIMEs, c(2018, 2020))
  expect_false("Year" %in% names(out))
})

test_that("data without an intertemporal dimension is returned as is", {
  sets <- list(ALLTIME = int_set(), REG = character(0))
  dt <- data.table::data.table(REGr = c("a", "b"), Value = 1:2)
  out <- .match_year(dt, sets, time_steps)
  expect_identical(names(out), c("REGr", "Value"))
})
