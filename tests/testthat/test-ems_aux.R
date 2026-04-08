skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

temp_dir <- withr::local_tempdir()

dat_input <- Sys.getenv("GTAP11c_dat")
par_input <- Sys.getenv("GTAP11c_par")
set_input <- Sys.getenv("GTAP11c_set")
har_input <- file.path(dirname(Sys.getenv("GTAP11c_dat")), "gdpextra.har")

d <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "full",
  COMM = "full",
  ACTS = "full",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)
)

POP <- d$POP
pop_csv <- file.path(temp_dir, "pop.csv")
write.csv(POP, pop_csv, row.names = FALSE)

# --- error tests ---

test_that("ems_aux errors when input is missing", {
  expect_snapshot_error(ems_aux())
})

test_that("ems_aux errors when type is missing", {
  expect_snapshot_error(ems_aux(input = POP))
})

test_that("ems_aux errors when header is missing for data frame input", {
  expect_snapshot_error(ems_aux(input = POP, type = "dat"))
})

test_that("ems_aux errors when header is missing for CSV input", {
  expect_snapshot_error(ems_aux(input = pop_csv, type = "dat"))
})

test_that("ems_aux errors when input is numeric", {
  expect_snapshot_error(ems_aux(input = 1, type = "dat", header = "POP"))
})

test_that("ems_aux errors when input is logical", {
  expect_snapshot_error(ems_aux(input = TRUE, type = "par", header = "POP"))
})

test_that("ems_aux errors when data frame lacks Value column", {
  no_val <- POP
  no_val$Value <- NULL
  expect_snapshot_error(ems_aux(input = no_val, type = "dat", header = "POP"))
})

# --- acceptance tests ---

test_that("ems_aux accepts data frame", {
  result <- ems_aux(input = POP, type = "dat", header = "POP")
  expect_s3_class(result, "array")
})

test_that("ems_aux accepts CSV path", {
  result <- ems_aux(input = pop_csv, type = "dat", header = "POP")
  expect_s3_class(result, "array")
})

test_that("ems_aux accepts HAR path without header", {
  result <- ems_aux(input = har_input, type = "par")
  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("ems_aux result has call attribute", {
  result <- ems_aux(input = POP, type = "dat", header = "POP")
  expect_false(is.null(attr(result, "call")))
})

test_that("ems_aux elements carry header and data type in class", {
  result <- ems_aux(input = POP, type = "dat", header = "POP")
  expect_true(inherits(result, "POP"))
  expect_true(inherits(result, "dat"))
})

# --- ems_data integration tests ---

test_that("ems_data accepts single ems_aux output as aux_input", {
  aux <- ems_aux(input = POP, type = "dat", header = "POP")

  .data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3)
  )

  expect_type(.data, "list")
  expect_true(length(.data) > 0)
})

test_that("ems_data accepts list of ems_aux outputs as aux_input", {
  aux_pop <- ems_aux(input = pop_csv, type = "dat", header = "POP")
  aux_har <- ems_aux(input = har_input, type = "par")
  
  .data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = list(aux_pop, aux_har),
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3)
  )

  expect_type(.data, "list")
  expect_true(length(.data) > 0)
})

test_that("ems_data errors when aux data has wrong dimensions", {
  wrong_dim <- data.frame(
    REG   = c("usa", "chn"),
    extra = c("a", "b"),
    Value = c(1, 2)
  )
  aux <- ems_aux(input = wrong_dim, type = "dat", header = "POP")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3)
  ))
})

test_that("ems_data errors when aux data is missing elements for replacement header", {
  extra_pop <- POP
  extra_pop$REG[1] <- "nonexistent_region"
  aux <- ems_aux(input = extra_pop, type = "dat", header = "POP")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3)
  ))
})

test_that("ems_data errors when aux data is missing elements for new header", {
  extra_pop <- POP
  extra_pop$REG[1] <- "nonexistent_region"
  aux <- ems_aux(input = extra_pop, type = "dat", header = "new_header")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3)
  ))
})


test_that("ems_data issues warning when type is different for replacement header", {
  wrong_type <- POP
  aux <- ems_aux(input = wrong_type, type = "par", header = "POP")
  expect_snapshot_warning(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3)
  ))
})

test_that("ems_data issues warning when renaming aux data set with recognized ele", {
  wrong_set <- POP
  colnames(wrong_set)[1] <- "population"
  aux <- ems_aux(input = wrong_set, type = "dat", header = "new_header")
  expect_snapshot_warning(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3)
  ))
})

test_that("ems_data issues warning when changing name on new header with recognized ele", {
  new_header <- POP
  colnames(new_header)[1] <- "RREG"
  aux <- ems_aux(input = new_header, type = "dat", header = "new_header")
  expect_snapshot_warning(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3)
  ))
})

test_that("ems_data issues warning when changing name on replacement header with recognized ele", {
  header <- POP
  colnames(header)[1] <- "RREG"
  aux <- ems_aux(input = header, type = "dat", header = "POP")
  expect_snapshot_warning(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3)
  ))
})

test_that("ems_data errors when unrecognized set and incomplete ele", {
  wrong_set <- POP
  colnames(wrong_set)[1] <- "population"
  wrong_set$population[1] <- "nonexistent_region"
  aux <- ems_aux(input = wrong_set, type = "dat", header = "new_header")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3)
  ))
})

test_that("ems_data accepts new set", {
  ele <- c("prodtax", "pfacttax", "inctax", "inputtax", "contax", "invtax", "govtax", "xtax", "mtax")
  aux_set <- ems_aux(
    input = ele,
    type = "set",
    header = "ALLTAX"
  )

  .data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux_set,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3)
  )

  result <- data.table::data.table(
    origin = ele,
    mapping = ele,
    key = c("mapping", "origin")
  )
  
  expect_true(isTRUE(all.equal(.data$ALLTAX, result, check.attributes = FALSE)))
})

test_that("ems_data errors when aux data has unrecognizable set", {
  bad_set <- data.frame(
    FAKE_SET = c("a", "b", "c"),
    Value    = c(1, 2, 3)
  )
  aux <- ems_aux(input = bad_set, type = "dat", header = "rndm")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    aux_input = aux,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3)
  ))
})