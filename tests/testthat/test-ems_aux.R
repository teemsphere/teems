skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

temp_dir <- withr::local_tempdir()

dat_input <- "~/dat/GTAP/v10A/flexagg10AY14/gsddat.har"
par_input <- "~/dat/GTAP/v10A/flexagg10AY14/gsdpar.har"
set_input <- "~/dat/GTAP/v10A/flexagg10AY14/gsdset.har"
har_input <- "~/dat/GDYN/GDYNflexagg10A_Y14/gdpextra.har"

d <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "full",
  TRAD_COMM = "full",
  ENDW_COMM = "labor_agg",
  time_steps = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)
)

pop <- d$POP
VTWR <- d$VTWR

class(pop) <- "data.frame"
pop_csv <- file.path(temp_dir, "pop.csv")
write.csv(pop, pop_csv, row.names = FALSE)

SAVE <- pop
SAVE$Value <- runif(nrow(SAVE))

test_that("ems_aux errors when no inputs provided", {
  expect_snapshot_error(ems_aux())
})

test_that("ems_aux errors when dat is numeric", {
  expect_snapshot_error(ems_aux(dat = 1))
})

test_that("ems_aux errors when par is logical", {
  expect_snapshot_error(ems_aux(par = TRUE))
})

test_that("ems_aux errors when set is numeric", {
  expect_snapshot_error(ems_aux(set = 1))
})


test_that("ems_aux errors when set is provided", {
  set_df <- data.frame(REG = c("usa", "chn", "row"))
  expect_snapshot_error(ems_aux(set = list(REG = set_df)))
})

test_that("ems_aux errors when list element is not character or data.frame", {
  expect_snapshot_error(ems_aux(dat = list(POP = 1)))
})

test_that("ems_aux errors when data frame lacks Value column", {
  no_val <- pop
  no_val$Value <- NULL
  expect_snapshot_error(ems_aux(dat = list(POP = no_val)))
})

test_that("ems_aux accepts named data frame for dat", {
  result <- ems_aux(dat = list(SAVE = SAVE))
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "array"))
})

test_that("ems_aux accepts named data frame for par", {
  result <- ems_aux(par = list(POP = pop))
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "array"))
})

test_that("ems_aux accepts named CSV path for dat", {
  result <- ems_aux(dat = list(POP = pop_csv))
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "array"))
})

test_that("ems_aux accepts named CSV path for par", {
  result <- ems_aux(par = list(POP = pop_csv))
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "array"))
})

test_that("ems_aux accepts unnamed HAR path", {
  result <- ems_aux(par = list(har_input))
  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("ems_aux accepts multiple named items for dat", {
  result <- ems_aux(dat = list(POP = pop_csv, SAVE = SAVE, VTWR = VTWR))
  expect_type(result, "list")
  expect_length(result, 3)
})

test_that("ems_aux accepts mixed CSV, data frame, and HAR inputs", {
  result <- ems_aux(
    dat = list(POP = pop_csv, SAVE = SAVE, VTWR = VTWR, rndm = VTWR),
    par = list(POP = pop_csv, har_input)
  )
  expect_type(result, "list")
  expect_true(length(result) > 4)
})

test_that("ems_aux result has call attribute", {
  result <- ems_aux(dat = list(POP = pop))
  expect_false(is.null(attr(result, "call")))
})

test_that("ems_aux dat elements carry header and data type in class", {
  result <- ems_aux(dat = list(POP = pop))
  expect_true("POP" %in% class(result[[1]]))
  expect_true("dat" %in% class(result[[1]]))
})

test_that("ems_data accepts ems_aux output as aux_input", {
  aux <- ems_aux(
    dat = list(POP = pop_csv, SAVE = SAVE, VTWR = VTWR, rndm = VTWR),
    par = list(POP = pop_csv, har_input)
  )
  
  .data <- suppressWarnings(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16),
    target_format = "GTAPv7"
  ))

  expect_type(.data, "list")
  expect_true(length(.data) > 0)
})

test_that("ems_data errors when aux data has wrong dimensions", {
  wrong_dim <- data.frame(
    REG = c("usa", "chn"),
    extra = c("a", "b"),
    Value = c(1, 2)
  )
  aux <- ems_aux(dat = list(POP = wrong_dim))
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    TRAD_COMM = "full",
    ENDW_COMM = "labor_agg",
    time_steps = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)
  ))
})

test_that("ems_data errors when aux data is missing elements", {
  missing_pop <- pop
  missing_pop$REG[1] <- "nonexistent_region"
  aux <- ems_aux(dat = list(POP = missing_pop))
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "full",
    TRAD_COMM = "full",
    ENDW_COMM = "labor_agg",
    time_steps = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)
  ))
})

test_that("ems_data errors when aux data has unrecognizable set", {
  bad_set <- data.frame(
    FAKE_SET = c("a", "b", "c"),
    Value = c(1, 2, 3)
  )
  aux <- ems_aux(dat = list(rndm = bad_set))
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    TRAD_COMM = "full",
    ENDW_COMM = "labor_agg",
    time_steps = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)
  ))
})
