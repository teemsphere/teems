#' Ask an image for its solver version. `NA_character_` when the binary
#' gives no `teems-solver <version>` line (an image that predates the
#' flag runs the solver proper instead: PETSc's own `-version` banner,
#' then `Error: cannot open ./reg.cmf`).
#'
#' @keywords internal
#' @noRd
.solver_version_query <- function(image) {
  out <- tryCatch(
    suppressWarnings(system2("docker",
      c(
        "run", "--rm", "--entrypoint", "/opt/teems-solver/solver/teems-solver",
        image, "-version"
      ),
      stdout = TRUE, stderr = TRUE
    )),
    error = \(e) character(0)
  )
  hit <- regmatches(out, regexpr("^teems-solver [0-9]+\\.[0-9]+\\.[0-9]+\\S*", out))
  if (!length(hit)) {
    return(NA_character_)
  }
  version <- sub("^teems-solver ", "", hit[1])
  return(version)
}
