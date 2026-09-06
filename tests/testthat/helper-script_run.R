# Source one example script from inst/scripts/<model> in its own
# environment, with the inputs the scripts expect as free variables, and
# return the logical checks it defines (`check`, `checks`, `var_check`,
# `coeff_check`); a non-logical check (an all.equal message) is FALSE.
run_script <- function(model,
                       name,
                       inputs,
                       model_files,
                       write_dir,
                       services = NULL) {
  path <- system.file("scripts", model, name, package = "teems")
  if (!nzchar(path)) {
    stop("Script not found: ", file.path(model, name))
  }

  tempdir <- file.path(write_dir, tools::file_path_sans_ext(name))
  dir.create(tempdir)
  ems_option_set(tempdir = tempdir)

  env <- new.env(parent = globalenv())
  env$dat_input <- inputs$dat
  env$par_input <- inputs$par
  env$set_input <- inputs$set
  env$year <- inputs$year
  env$model_file <- model_files[["model_file"]]
  env$closure_file <- model_files[["closure_file"]]
  env$services <- services
  source(path, local = env)

  checks <- mget(
    c("check", "checks", "var_check", "coeff_check"),
    envir = env,
    ifnotfound = list(NULL)
  )
  checks <- Filter(Negate(is.null), checks)
  unlist(lapply(checks, function(x) if (is.logical(x)) x else FALSE))
}

nest_temp <- function(name,
                      write_dir) {
  tempdir <- file.path(write_dir, name)
  dir.create(tempdir)
  ems_option_set(tempdir = tempdir)
}

write_modified_model <- function(model_file, text, .fn = paste) {
  model_text <- readChar(model_file, file.info(model_file)[["size"]])
  modified <- .fn(model_text, text)
  out_path <- file.path(dirname(model_file), "error.tab")
  writeLines(modified, out_path)
  out_path
}

write_modified_closure <- function(closure_file, text, .fn = cat) {
  closure_text <- readLines(closure_file)
  modified <- capture.output(.fn(closure_text[[1]], text, tail(closure_text, -1), sep = "\n"))
  out_path <- file.path(dirname(closure_file), "error.cls")
  writeLines(modified, out_path)
  out_path
}

ems_test_dir <- function(write_dir, name) {
  test_dir <- file.path(write_dir, name)
  if (dir.exists(test_dir)) {
    unlink(list.dirs(test_dir, recursive = FALSE), recursive = TRUE)
  } else {
    dir.create(test_dir, recursive = TRUE)
  }
  return(test_dir)
}