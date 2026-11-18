#' @importFrom purrr map_chr map2
#' @keywords internal
#' @noRd
.shk_load <- function(shocks,
                      closure,
                      sets,
                      var_extract) {

  final_shocks <- lapply(
    X = shocks,
    FUN = \(shk) {
      .construct_shk(
        raw_shock = shk,
        closure = closure,
        sets = sets,
        var_extract = var_extract
      )
    }
  )

  tuples <- purrr::map2(shocks, final_shocks, \(shk, fin) {
    if (inherits(shk, "uniform")) {
      .shk_uniform_tuples(shk = shk, sets = sets)
    } else {
      attr(fin, "tuples")
    }
  })
  .chk_shk_repeat(shocks = shocks, tuples = tuples)

  final_shocks <- unlist(x = final_shocks, recursive = FALSE)
  shock_names <- unique(x = purrr::map_chr(.x = shocks, .f = "var"))
  shock_id <- strtrim(x = paste(shock_names, collapse = "_"), width = 60)

  file <- paste0(shock_id, ".shf")
  attr(final_shocks, "file") <- file

  return(final_shocks)
}
