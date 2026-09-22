#' @importFrom purrr compact map_lgl
#' @keywords internal
#' @noRd
.process_data <- function(i_data,
                          set_mappings,
                          call) {

  metadata <- attr(i_data, "metadata")
  set_raw <- lapply(
    i_data[purrr::map_lgl(i_data, is.character)],
    \(h) tolower(trimws(unclass(h)))
  )
  nm_order <- names(i_data)
  is_arr <- purrr::map_lgl(i_data, \(x) {
    inherits(x, "dat") && is.numeric(x) &&
      !is.null(dimnames(x)) && !is.null(names(dimnames(x)))
  })
  arr_data <- i_data[is_arr]
  dt_data <- .array2DT(i_data = i_data[!is_arr])

  weight_headers <- gsub("-", "", unique(unlist(param_weights[[metadata$data_format]])))
  weights <- c(arr_data, dt_data)
  weights <- weights[names(weights) %in% weight_headers]

  dt_data <- .weight_param(
    i_data = dt_data,
    weights = weights,
    data_format = metadata$data_format
  )

  ndigits <- .o_ndigits()
  dt_agg <- lapply(dt_data,
    .aggregate_data,
    sets = set_mappings,
    ndigits = ndigits
  )
  arr_agg <- lapply(arr_data,
    .aggregate_array,
    sets = set_mappings,
    ndigits = ndigits
  )
  i_data <- c(dt_agg, arr_agg)[nm_order]
  names(i_data) <- nm_order
  i_data <- purrr::compact(i_data)
  attr(i_data, "metadata") <- metadata
  attr(i_data, "call") <- call
  attr(i_data, "set_raw") <- set_raw
  if ("time_steps" %in% names(attributes(set_mappings))) {
    attr(i_data, "time_steps") <- attr(set_mappings, "time_steps")
  }
  return(i_data)
}
