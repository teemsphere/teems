skip_on_cran()

write_dir <- withr::local_tempdir()
model_files <- ems_example("GTAPv7", write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

with_qfd_level <- function(level) {
  text <- readLines(model_file)
  text <- sub("Variable (orig_level=VDFB)", paste0("Variable (orig_level=", level, ")"), text, fixed = TRUE)
  out <- file.path(write_dir, "orig.tab")
  writeLines(text, out)
  out
}

test_that("an ORIG_LEVEL naming an undeclared coefficient aborts", {
  expect_snapshot_error(ems_model(with_qfd_level("NOSUCH"), closure_file))
})

test_that("an ORIG_LEVEL coefficient over other sets aborts", {
  expect_snapshot_error(ems_model(with_qfd_level("VDGB"), closure_file))
})

test_that("an integer ORIG_LEVEL coefficient aborts", {
  text <- readLines(with_qfd_level("NINT"))
  text <- c(text, "Coefficient (integer)(all,c,COMM)(all,a,ACTS)(all,r,REG) NINT(c,a,r) # int #;",
            "Formula (all,c,COMM)(all,a,ACTS)(all,r,REG) NINT(c,a,r) = 1;")
  writeLines(text, file.path(write_dir, "orig.tab"))
  expect_snapshot_error(ems_model(file.path(write_dir, "orig.tab"), closure_file))
})

test_that("the vetted models' ORIG_LEVELs pass and their variables are typed", {
  model <- ems_model(model_file, closure_file)
  vpq <- attr(model, "vpqtype")
  expect_true(length(vpq) > 0L)
  expect_true(all(vpq %in% c("value", "price", "quantity", "none", "unspecified")))
})
