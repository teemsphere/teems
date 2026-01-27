#' @importFrom data.table copy rbindlist let setnames
#' 
#' @keywords internal
#' @noRd
.weight_param <- function(i_data,
                          data_format) {

  # NSE
  omega <- Value <- NULL
  
  weight_map <- switch(data_format,
                       "GTAPv6" = param_weights$GTAPv6,
                       "GTAPv7" = param_weights$GTAPv7)

  weight_headers <- gsub("-", "", unique(unlist(weight_map)))
  weights <- data.table::copy(i_data[names(i_data) %in% weight_headers])

  if (data_format %=% "GTAPv7") {
    data.table::setnames(weights$ISEP, old = "COMM", new = "ACTS")
  }

  flip_headers <- sub("-", "", grep("-", unlist(weight_map), value = TRUE))
  weights <- lapply(weights, function(w) {
    if (inherits(w, flip_headers)) {
      w[, let(Value = Value * -1)]
    }
    return(w)
  })

  weight_map <- lapply(weight_map, gsub, pattern = "-", replacement = "")

  i_data <- lapply(i_data, function(h) {
    if (inherits(h, names(weight_map))) {
      w_headers <- weight_map[[class(h)[1]]]
      ls_w <- weights[w_headers]
      sets <- colnames(h)[!colnames(h) %in% "Value"]
      w <- data.table::rbindlist(lapply(ls_w, function(weight) {
        weight[, list(Value = sum(Value)), by = sets]
      }))[, list(omega = sum(Value)), by = sets]
      h <- merge(h, w, sets)
      h[, let(sigma = Value * omega)]
    }
    return(h)
  })
 
  return(i_data)
}
