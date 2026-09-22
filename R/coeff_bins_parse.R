#' @importFrom tibble as_tibble
#' @importFrom data.table data.table
#' @keywords internal
#' @noRd
.has_coefficient_dump <- function(sol_prefix) {
  return(file.exists(paste0(sol_prefix, "cof")) &&
    file.exists(paste0(sol_prefix, "cbin")))
}

#' @importFrom tibble as_tibble
#' @importFrom data.table data.table
#' @keywords internal
#' @noRd
.parse_coefficient_bins <- function(sol_prefix,
                                    coeff_names = character(0),
                                    read_values = TRUE) {
  raw <- parse_coefficients(sol_prefix, coeff_names, read_values)

  cof_union <- tibble::as_tibble(data.frame(
    r_idx     = seq_along(raw$cof$cofname) - 1L,
    cofname   = raw$cof$cofname,
    begadd    = raw$cof$begadd,
    size      = raw$cof$size,
    setid     = raw$cof$setid,
    antidims  = raw$cof$antidims,
    matsize   = raw$cof$matsize,
    postsim   = raw$cof$postsim,
    parameter = raw$cof$parameter,
    stringsAsFactors = FALSE
  ))
  cof_union$pack_begadd <- c(0, cumsum(cof_union$matsize))[seq_len(nrow(cof_union))]

  xc <- if (read_values) {
    data.table::data.table(
      r_idx = seq_along(raw$bin) - 1L,
      Value = raw$bin
    )
  } else {
    NULL
  }

  bins <- list(
    cof_union = cof_union,
    xc        = xc
  )
  return(bins)
}
