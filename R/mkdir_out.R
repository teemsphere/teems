#' @keywords keyword
#' @noRd
.out_mkdir <- function(write_dir) {
  sets_out <- file.path(write_dir, "out", "sets")
  if (!dir.exists(sets_out)) {
    dir.create(sets_out,
      recursive = TRUE
    )
  }

  coeff_out <- file.path(write_dir, "out", "coefficients")
  if (!dir.exists(coeff_out)) {
    dir.create(coeff_out,
      recursive = TRUE
    )
  }

  var_out <- file.path(write_dir, "out", "variables", "bin")
  if (!dir.exists(var_out)) {
    dir.create(var_out,
      recursive = TRUE
    )
  }
  return(invisible(NULL))
}