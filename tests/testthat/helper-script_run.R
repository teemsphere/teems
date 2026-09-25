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
  quiet_pivot(source(path, local = env))

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
# Snapshot transform for messages that embed a path or a run-specific
# name. The user cache root (tools::R_user_dir) made every such snapshot
# user- and OS-specific: variants keyed by sysname were really keyed by
# the home directory, so any other user, WSL2 or a fresh laptop
# regenerated them on every run and the stored Windows/Linux sets could
# never be diffed cleanly. Scrubbing the root (in the forms R prints it,
# including the mixed separators R_user_dir yields on Windows) and the
# per-run solver log name and image tag leaves one platform-independent
# snapshot set.
scrub_paths <- function(lines) {
  cache <- tools::R_user_dir("teems", "cache")
  forms <- unique(c(
    cache,
    normalizePath(cache, winslash = "/", mustWork = FALSE),
    normalizePath(cache, winslash = "\\", mustWork = FALSE),
    gsub("/", "\\", cache, fixed = TRUE)
  ))
  for (p in forms[nzchar(forms)]) {
    lines <- gsub(p, "<cache>", lines, fixed = TRUE)
  }
  # separators below the scrubbed root: the package builds its paths with
  # file.path() over R_user_dir() and normalises with an explicit forward
  # slash, so Windows renders them the same way Linux does, but a message
  # that reached plain normalizePath() would carry backslashes and differ
  # for that alone. Only the run of path characters after <cache> is
  # touched, so nothing else in the message is rewritten.
  lines <- vapply(
    lines,
    function(x) {
      while (grepl("<cache>[^\\\\\"']*\\\\", x)) {
        x <- sub("(<cache>[^\\\\\"']*)\\\\", "\\1/", x)
      }
      x
    },
    character(1),
    USE.NAMES = FALSE
  )
  # the docker --mount value is quoted for the platform's shell (single
  # quotes under sh, double under cmd); the quoting is not what these
  # snapshots are about, and leaving it in would make them OS-specific
  # again, which is exactly what the scrubbing removed
  lines <- gsub("--mount ['\"](type=bind[^'\"]*)['\"]", "--mount \\1", lines)
  # Linux hosts run the container as the calling user (--user uid:gid);
  # Docker Desktop hosts do not, so the flag is dropped here
  lines <- gsub("docker run --rm --user [0-9]+:[0-9]+ -e HOME=/tmp ", "docker run --rm ", lines)
  lines <- gsub("solver_out_[0-9]+(_[0-9]+)?\\.txt", "solver_out_HHMM.txt", lines)
  gsub("teems:[A-Za-z0-9._-]+ /bin/bash", "teems:TAG /bin/bash", lines)
}

# Muffle the condensation warning that a backsolve pivot divides by a
# coefficient expression (model_wrn$condense_pivot_zero): the GTAPv7 and
# GTAP-RE models raise it on every ems_model() call, and
# test-ems_model.R "backsolve through a coefficient pivot synthesizes a
# reciprocal and warns" is the one test that asserts it fires.
quiet_pivot <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("divides by the coefficient expression", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}
