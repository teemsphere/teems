#!/usr/bin/env Rscript
# Every formal check before a CRAN submission, in one run, from the package root:
#
#   Rscript dev/cran_checks.R              the local checks (about 5 minutes)
#   Rscript dev/cran_checks.R --suite      ... plus the full test suite with the solver (about 1.5 h)
#   Rscript dev/cran_checks.R --remote     ... plus the remote builders (win-builder, mac-builder, R-hub)
#
# Each step prints PASS, FAIL or NOTE and the table at the end is what cran-comments.md
# reports. The exit status is 1 when any step fails. Follows the ThinkR checklist
# (github.com/ThinkR-open/prepare-for-cran) minus the steps that do not apply: no
# reverse dependencies yet, dependencies are maintained by hand (@importFrom per
# function), no vignettes.
#
# The R CMD check step runs under CRAN's conditions: NOT_CRAN unset, an empty user
# Renviron (no GTAP*/GDYN*/ORANIG database variables), and a `docker` on the PATH that
# fails, so every test and example that needs the solver or a database has to skip
# by itself, as it will on CRAN.

args <- commandArgs(trailingOnly = TRUE)
want_suite <- "--suite" %in% args
want_remote <- "--remote" %in% args
pkg <- normalizePath(".")
if (!file.exists(file.path(pkg, "DESCRIPTION")) || !dir.exists(file.path(pkg, "dev"))) {
  stop("run from the teems-R package root")
}
desc <- read.dcf(file.path(pkg, "DESCRIPTION"))
version <- desc[1, "Version"]
scratch <- file.path(tempdir(), "cran_checks"); dir.create(scratch, showWarnings = FALSE)
results <- list()
step <- function(name, expr) {
  t0 <- Sys.time()
  r <- tryCatch(force(expr), error = function(e) structure(conditionMessage(e), class = "step_fail"))
  status <- if (inherits(r, "step_fail")) "FAIL" else if (is.character(r) && length(r) == 1L && startsWith(r, "NOTE")) "NOTE" else "PASS"
  detail <- if (status == "PASS") if (is.character(r) && length(r) == 1L) r else "" else as.character(r)
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  cat(sprintf("%-4s %-44s %4d s  %s\n", status, name, secs, sub("\n.*", "", detail)))
  if (nzchar(detail) && grepl("\n", detail)) cat(paste0("     ", strsplit(detail, "\n")[[1]][-1], "\n"), sep = "")
  results[[name]] <<- list(status = status, detail = detail, secs = secs)
  invisible(r)
}
cat(sprintf("teems %s at %s, R %s, %s\n\n", version, pkg, getRversion(), format(Sys.time(), "%Y-%m-%d %H:%M %Z")))

## 1. the tree -----------------------------------------------------------------------
step("git tree clean (nothing uncommitted)", {
  st <- system2("git", c("-C", shQuote(pkg), "status", "--porcelain"), stdout = TRUE)
  if (length(st) > 0L) paste0("NOTE: ", length(st), " uncommitted path(s): ", paste(head(st, 5), collapse = "; ")) else
    paste("HEAD", system2("git", c("-C", shQuote(pkg), "rev-parse", "--short", "HEAD"), stdout = TRUE))
})
step("roxygen output up to date (man/, NAMESPACE)", {
  before <- system2("git", c("-C", shQuote(pkg), "status", "--porcelain", "man", "NAMESPACE"), stdout = TRUE)
  suppressMessages(roxygen2::roxygenise(pkg, load_code = roxygen2::load_pkgload))
  after <- system2("git", c("-C", shQuote(pkg), "status", "--porcelain", "man", "NAMESPACE"), stdout = TRUE)
  if (!identical(before, after)) stop("roxygenise() changed: ", paste(setdiff(after, before), collapse = "; "))
  "no change"
})
step("every function tagged @export or @noRd, @return present", {
  f <- suppressMessages(checkhelper::audit_tags(pkg))$functions
  bad <- f[f$test_has_export_or_has_nord != "ok" | f$test_has_export_and_return != "ok", , drop = FALSE]
  if (nrow(bad) > 0L) stop(nrow(bad), " function(s): ", paste(head(bad$topic, 6), collapse = ", "))
  paste(nrow(f), "documented functions")
})

step("no compiled objects left in src/", {
  stale <- list.files(file.path(pkg, "src"), pattern = "\\.(o|so|dll|dylib)$", full.names = TRUE)
  if (length(stale) > 0L) { unlink(stale); paste("removed", length(stale)) } else "none"
})

## 2. prose --------------------------------------------------------------------------
step("spelling (inst/WORDLIST)", {
  sp <- spelling::spell_check_package(pkg)
  if (nrow(sp) > 0L) paste0("NOTE: ", nrow(sp), " unknown word(s): ", paste(head(sp$word, 12), collapse = ", "),
                            if (nrow(sp) > 12L) " ..." else "") else "0 unknown words"
})
step("URLs resolve (DESCRIPTION, Rd, README)", {
  u <- urlchecker::url_check(pkg)
  if (nrow(u) > 0L) paste0("NOTE: ", nrow(u), " URL problem(s):\n", paste(u$URL, u$Status, u$Message, sep = "  ", collapse = "\n")) else "0 problems"
})
step("DESCRIPTION fields CRAN reads", {
  need <- c("Title", "Description", "Version", "Authors@R", "License", "URL", "BugReports", "Encoding")
  miss <- need[!need %in% colnames(desc)]
  if (length(miss) > 0L) stop("missing: ", paste(miss, collapse = ", "))
  if (!grepl("cph", desc[1, "Authors@R"])) stop("no copyright holder (cph) in Authors@R")
  if (grepl("^(This package|teems)", desc[1, "Description"])) stop("Description starts with the package name")
  if (grepl(" (in|with) R\\b", desc[1, "Title"])) stop("Title says 'in R'")
  paste("version", version)
})

## 3. the package under CRAN's conditions ---------------------------------------------
cran_env <- function() {
  shim <- file.path(scratch, "noshim"); dir.create(shim, showWarnings = FALSE)
  writeLines(c("#!/bin/sh", "exit 1"), file.path(shim, "docker")); Sys.chmod(file.path(shim, "docker"), "755")
  empty <- file.path(scratch, "empty.Renviron"); file.create(empty)
  c(NOT_CRAN = "", R_ENVIRON_USER = empty, PATH = paste(shim, Sys.getenv("PATH"), sep = .Platform$path.sep),
    `_R_CHECK_FORCE_SUGGESTS_` = "false", `_R_CHECK_CRAN_INCOMING_REMOTE_` = "true")
}
tarball <- NULL
step("R CMD build", {
  old <- setwd(scratch); on.exit(setwd(old))
  out <- system2("R", c("CMD", "build", "--no-build-vignettes", shQuote(pkg)), stdout = TRUE, stderr = TRUE)
  tb <- list.files(scratch, pattern = "^teems_.*\\.tar\\.gz$", full.names = TRUE)
  if (length(tb) != 1L) stop(paste(tail(out, 5), collapse = "\n"))
  tarball <<- tb
  files <- untar(tb, list = TRUE)
  stray <- grep("\\.(o|so|Rhistory|DS_Store)$|/\\.audit/|/dev/|/data-raw/", files, value = TRUE)
  if (length(stray) > 0L) stop("stray files in the tarball: ", paste(head(stray, 5), collapse = ", "))
  sprintf("%s, %.1f MB, %d files", basename(tb), file.size(tb) / 1e6, length(files))
})
check <- NULL
step("R CMD check --as-cran with tests, CRAN conditions", {
  if (is.null(tarball)) stop("no tarball")
  check <<- rcmdcheck::rcmdcheck(tarball, args = c("--as-cran", "--no-manual"), env = cran_env(), quiet = TRUE,
                                 check_dir = file.path(scratch, "check"), timeout = 3600)
  size <- grep("installed size is", check$stdout, value = TRUE)
  tests <- grep("\\[ FAIL [0-9]+ \\| WARN", readLines(list.files(file.path(scratch, "check"), "testthat\\.Rout", recursive = TRUE, full.names = TRUE)[1], warn = FALSE), value = TRUE)
  if (length(check$errors) > 0L) stop("errors:\n", paste(check$errors, collapse = "\n"))
  if (length(check$warnings) > 0L) stop("warnings:\n", paste(check$warnings, collapse = "\n"))
  msg <- sprintf("0 errors, 0 warnings, %d note(s); %s; size %s", length(check$notes), tail(tests, 1), if (length(size)) trimws(size) else "under the threshold")
  if (length(check$notes) > 0L) paste0("NOTE: ", msg, "\n", paste(gsub("\n", "\n  ", check$notes), collapse = "\n")) else msg
})
step("tests under CRAN conditions reached the skips, not the solver", {
  rout <- list.files(file.path(scratch, "check"), "testthat\\.Rout", recursive = TRUE, full.names = TRUE)[1]
  if (is.na(rout)) stop("no test transcript")
  x <- readLines(rout, warn = FALSE)
  fails <- grep("^\\[ FAIL ([1-9]|[0-9]{2,})", x, value = TRUE)
  if (length(fails) > 0L) stop(fails[1])
  if (any(grepl("docker|teems-solver", x) & grepl("Error", x))) stop("a test reached docker")
  sub(".*(\\[ FAIL.*\\]).*", "\\1", tail(grep("\\[ FAIL", x, value = TRUE), 1))
})
step("examples run (with donttest)", {
  out <- utils::capture.output(suppressMessages(
    devtools::run_examples(pkg, run_donttest = TRUE, run_dontrun = FALSE, document = FALSE, fresh = TRUE)
  ), type = "output")
  n <- grep("^── Running [0-9]+ example files", out, value = TRUE)
  if (length(n)) sub("^── Running ([0-9]+) example files.*", "\\1 example files", n[1]) else "ok"
})
step("installed tarball: citation() prints this version", {
  if (is.null(tarball)) stop("no tarball")
  lib <- file.path(scratch, "lib"); dir.create(lib, showWarnings = FALSE)
  out <- system2("R", c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(tarball)), stdout = TRUE, stderr = TRUE)
  if (!any(grepl("DONE", out))) stop(paste(tail(out, 5), collapse = "\n"))
  cit <- system2("Rscript", c("-e", shQuote(sprintf('library(teems, lib.loc = "%s"); x <- citation("teems"); cat(format(x, style = "text")); cat("\n"); cat(toBibtex(x), sep = "\n")', lib))), stdout = TRUE, stderr = TRUE)
  if (!any(grepl(version, cit, fixed = TRUE))) stop("citation() does not print ", version, ":\n", paste(cit, collapse = "\n"))
  paste("citation() and toBibtex() show", version)
})

## 4. the real suite, with the solver (opt-in) ---------------------------------------
if (want_suite) {
  step("full test suite with the solver (NOT_CRAN=true)", {
    Sys.setenv(NOT_CRAN = "true")
    r <- as.data.frame(testthat::test_local(pkg, reporter = "silent", stop_on_failure = FALSE))
    bad <- r[r$failed > 0 | r$error, "test"]
    if (length(bad) > 0L) stop(length(bad), " failing: ", paste(head(bad, 5), collapse = "; "))
    sprintf("%d expectations, 0 failures, %d skipped", sum(r$passed), sum(r$skipped))
  })
} else cat("skip full test suite with the solver (run with --suite)\n")

## 5. remote builders (opt-in; results arrive by e-mail / on GitHub) ------------------
if (want_remote) {
  step("win-builder R-devel and R-release submitted", { devtools::check_win_devel(pkg, quiet = TRUE); devtools::check_win_release(pkg, quiet = TRUE); "submitted, results by e-mail" })
  step("mac-builder submitted", { devtools::check_mac_release(pkg, quiet = TRUE); "submitted, results by e-mail" })
  step("R-hub (GitHub Actions) launched", { rhub::rhub_check(platforms = c("linux", "windows", "macos", "clang-asan", "gcc-asan")); "launched on GitHub Actions" })
} else cat("skip remote builders (run with --remote): win-builder devel/release, mac-builder, R-hub linux/windows/macos/asan\n")
cat("skip reverse dependencies: none on CRAN yet\n")

## summary ---------------------------------------------------------------------------
cat("\n== summary for cran-comments.md\n")
for (n in names(results)) cat(sprintf("%-4s %s%s\n", results[[n]]$status, n, if (results[[n]]$status != "PASS") paste0(": ", sub("\n.*", "", results[[n]]$detail)) else ""))
if (!is.null(check)) cat(sprintf("\nR CMD check --as-cran: %d errors | %d warnings | %d notes\n", length(check$errors), length(check$warnings), length(check$notes)))
cat("check directory:", file.path(scratch, "check"), "\n")
quit(status = if (any(vapply(results, function(r) r$status == "FAIL", logical(1)))) 1L else 0L)
