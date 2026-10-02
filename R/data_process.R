#' @importFrom purrr compact map_lgl
#' @keywords internal
#' @noRd
.process_data <- function(i_data,
                          set_mappings,
                          par_weights,
                          call) {

  metadata <- attr(i_data, "metadata")
  set_raw <- lapply(
    i_data[purrr::map_lgl(i_data, is.character)],
    \(h) tolower(trimws(unclass(h)))
  )
  int_raw <- lapply(
    i_data[purrr::map_lgl(i_data, \(h) is.integer(h) && is.null(dimnames(h)))],
    \(h) as.vector(unclass(h))
  )
  nm_order <- names(i_data)
  is_arr <- purrr::map_lgl(i_data, \(x) {
    inherits(x, "dat") && is.numeric(x) &&
      !is.null(dimnames(x)) && !is.null(names(dimnames(x))) &&
      !exists(paste0(".aggregate_data.", class(x)[1]), mode = "function")
  })
  arr_data <- i_data[is_arr]
  dt_data <- .array2DT(i_data = i_data[!is_arr])

  if (metadata$data_format %in% names(param_weights$value)) {
    methods <- .resolve_par_weights(
      par_weights = par_weights,
      data_format = metadata$data_format,
      call = call
    )
    weight_entries <- c(
      unlist(param_weights$value[[metadata$data_format]]),
      unlist(lapply(param_weights$share[[metadata$data_format]], \(n) lapply(n, `[[`, "inputs")))
    )
    weight_headers <- unique(sub("\\[.*$", "", sub("^-", "", weight_entries)))
    weights <- c(arr_data, dt_data)
    weights <- weights[names(weights) %in% weight_headers]
    dt_data <- .weight_param(
      i_data = dt_data,
      weights = weights,
      sets = set_raw,
      set_mappings = set_mappings,
      methods = methods,
      data_format = metadata$data_format
    )
    metadata$par_weights <- methods
  }

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
  agg_data <- c(dt_agg, arr_agg)[nm_order]
  names(agg_data) <- nm_order
  i_data <- .fossil_vaen(
    agg_data = agg_data,
    i_data = i_data,
    set_raw = set_raw,
    set_mappings = set_mappings,
    metadata = metadata,
    ndigits = ndigits
  )
  i_data <- purrr::compact(i_data)
  attr(i_data, "metadata") <- metadata
  attr(i_data, "call") <- call
  attr(i_data, "set_raw") <- set_raw
  attr(i_data, "int_raw") <- int_raw
  if ("time_steps" %in% names(attributes(set_mappings))) {
    attr(i_data, "time_steps") <- attr(set_mappings, "time_steps")
  }
  return(i_data)
}
