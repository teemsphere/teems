# Contributing to teems

Thank you for your interest in teems. This page covers how to report a problem, how to ask for help, and how to contribute code or documentation.

teems is one of several repositories maintained by the [teemsphere](https://github.com/teemsphere) organization. Please direct contributions to the repository that holds the relevant component:

| Component | Repository |
|-----------|------------|
| R package (`teems`) | [teemsphere/teems](https://github.com/teemsphere/teems) |
| Solver (C/Fortran, Docker image) | [teemsphere/teems-solver](https://github.com/teemsphere/teems-solver) |
| Vetted model and closure files | [teemsphere/teems-models](https://github.com/teemsphere/teems-models) |
| Set mappings | [teemsphere/teems-mappings](https://github.com/teemsphere/teems-mappings) |
| User manual | [teemsphere/teems-manual](https://github.com/teemsphere/teems-manual) |

## Reporting a problem

Please open an issue at <https://github.com/teemsphere/teems/issues>. Before doing so, check the existing issues in case the problem has already been reported.

A useful report includes:

- the output of `packageVersion("teems")`, `R.version.string`, and your operating system;
- the solver image tag in use (`ems_option_get("docker_tag")`) and the output of `docker --version`;
- a minimal script that reproduces the problem, including the model, aggregation, and shocks used (GTAP databases are licensed and should not be attached; describe the version and aggregation instead);
- the complete console output, including the full text of any error message.

Error messages in teems are written to identify the offending element and the rule it breaks. If a message was unclear or pointed you in the wrong direction, that is worth reporting on its own.

## Seeking support

For questions about using the package, please first consult the [user manual](https://teemsphere.github.io/), which contains worked examples for each vetted model. If the manual does not answer your question, open an issue with the `question` label. The maintainer can also be reached at <matthew.cantele@protonmail.com>.

## Contributing code or documentation

Contributions are welcome through pull requests.

1. Open an issue first to describe the change you have in mind, so that it can be discussed before you invest time in it.
2. Fork the repository and create a branch from `dev`. Pull requests should target `dev`; `main` holds the released version.
3. Keep each pull request to a single change.
4. Run the test suite before submitting (see below) and add tests for new behavior. Tests of error messages use `testthat` snapshots.
5. Document exported functions with roxygen2 and regenerate the documentation with `devtools::document()`.

### Conventions

Code

- Exported functions are `ems_*`; internal functions are dot-prefixed (`.shk_load()`), one per file named after the function, documented with roxygen `@keywords internal` and `@noRd`.
- Every control statement takes braces (`if`, `else`, `for`, `while`), with the body on its own lines; no inline `if (x) a else b` assignments.
- A function builds a named object and ends with `return(obj)`, never `return(f(x))`; an S3 generic is a bare `UseMethod()`.
- Function bodies carry no comments; what needs explaining goes in the roxygen header.
- Caches are environments written with `env[["key"]] <- value`; `assign()` is not used.
- Every `pkg::fun()` call has a matching `@importFrom pkg fun` in the header of the function that uses it; there are no package-level imports.
- `%|||%` is the null-coalescing operator.
- Arguments are validated with `.cli_missing()` for a missing argument and `.check_arg_class()`/`.check_class()` for types; checks in `inst/scripts` accumulate logicals in a `checks` vector rather than calling `stopifnot()`.
- Solver and HSL controls keep the solver's own flag names (e.g. `ma48_cntl2`); names are not paraphrased.

Messages

- All user-facing text lives in `data-raw/R/msg_*.R`, grouped by category (`gen_err`, `model_err`, `shk_err`, `solve_err`, ...), and is built into `R/sysdata.rda` by `Rscript data-raw/teems_data-raw.R`, run from the package root (it reads the sibling `teems-mappings` and `teems-models` checkouts). Each message key is preceded by a comment naming the test that exercises it.
- Messages are dispatched through `.cli_action(msg, action = , call = call)`, use cli markup (`{.arg}`, `{.val}`, `{.code}`, `{.fun}`), and name the offending element and the rule it breaks.
- Plain language throughout: no working names or project-internal jargon in messages, argument names or outputs; US spelling, with domain terms listed in `inst/WORDLIST`.

Tests

- One `tests/testthat/test-<file>.R` per `R/<file>.R`. Aborts are tested with `expect_snapshot_error()`, and the snapshot files are committed.
- Files that need the GTAP data start with `skip_on_cran()` and `skip_if(!nzchar(Sys.getenv("GTAP12_dat")), "GTAP data not available")`; solver runs use the configured `docker_tag`.
- `test-examples.R` runs every example; a new example is added there.
- A commit that changes a message also carries the regenerated `R/sysdata.rda`; one that changes roxygen carries the regenerated `man/` and `NAMESPACE`. `Rscript dev/cran_checks.R` runs the pre-submission checks.

Commits

- The subject names the change; the body says what the user saw before, why, and what changes now. No tool names, no trailers.
- Please do not edit `NEWS.md`; release notes are assembled at release time.

### Running the tests

```r
devtools::test()
```

The suite has two parts. Tests of the parsers, checks, options, and solution-file readers are self-contained and run anywhere. Tests that build or solve a model need a licensed GTAP database, which cannot be distributed with the package, and a locally built solver image (see the [teems-solver](https://github.com/teemsphere/teems-solver) README). These tests are skipped with the reason `GTAP data not available` or a note about the missing image when either is absent, so a run without them reports passes and skips rather than failures.

You are not expected to run the full suite. Run what you can, add tests for your change, and the maintainer will run the complete suite, including the solver reference runs, before anything is merged or released.

If you do hold a GTAP 12 license, the data tests read the database through environment variables, which are most conveniently set in `.Renviron`:

```sh
GTAP12_dat=/path/to/gsdfdat.har
GTAP12_par=/path/to/gsdfpar.har
GTAP12_set=/path/to/gsdfset.har
```

Further variables of the same form (`GTAP12E_*`, `GTAP12P_*`, `GTAP12AEZ_*`, `GTAP11c_*`, `GTAP10A_*`, `GTAP9_*`, `GDYN11c_*`, `ORANIG_har`) feed the database-conversion and vetted-model tests and are skipped when unset.

The package is checked with `R CMD check` on Linux, macOS, and Windows through R-hub before each release; the workflow in `.github/workflows/rhub.yaml` can be run on a branch from the Actions tab.

## Contributing models and mappings

New vetted models, closures, and aggregation mappings belong in the `teems-models` and `teems-mappings` repositories respectively. Each has its own README describing the expected files and the checks a contribution must pass.

## Code of conduct

This project follows the [Contributor Covenant](CODE_OF_CONDUCT.md). By participating you agree to abide by its terms.
