#' Recast a COMM-dimensioned weight onto TOPP
#'
#' TOPP holds the commodities that are not energy plus a single node
#' standing for the energy composite, so the correspondence is read off
#' the two element lists: a commodity that is itself a TOPP element maps
#' to itself, and the rest carry the one TOPP element that is not a
#' commodity. The caller sums by TOPP, which performs the collapse.
#'
#' @keywords internal
#' @noRd
.weight_over_topp <- function(w, topp, e_header) {

  # NSE
  TOPP <- NULL

  if (is.null(w)) {
    e_dim <- "an absent header"
    .cli_action(data_err$e_topp_weight, action = "abort")
  }
  if (!is.data.frame(w)) {
    w <- .array2DT(i_data = list(w))[[1]]
  } else {
    w <- data.table::copy(w)
  }
  if (!"COMM" %in% colnames(w)) {
    e_dim <- paste(setdiff(colnames(w), "Value"), collapse = ", ")
    .cli_action(data_err$e_topp_weight, action = "abort")
  }
  data.table::setnames(w, old = "COMM", new = "TOPP")
  e_agg <- setdiff(topp, unique(as.character(w[["TOPP"]])))
  if (!length(e_agg) %=% 1L) {
    e_dim <- "COMM"
    .cli_action(data_err$e_topp_weight, action = "abort")
  }
  w[!TOPP %in% topp, let(TOPP = e_agg)]
  return(w)
}

#' @importFrom data.table copy rbindlist let setnames
#'
#' @keywords internal
#' @noRd
.weight_param <- function(i_data,
                          weights,
                          data_format) {

  # NSE
  omega <- Value <- TOPP <- NULL

  weight_map <- switch(data_format,
                       "GTAPv6" = param_weights$GTAPv6,
                       "GTAPv7" = param_weights$GTAPv7)

  if (data_format %=% "GTAPv7" && !is.null(weights$ISEP)) {
    w <- weights$ISEP
    if (is.data.frame(w)) {
      w <- data.table::copy(w)
      data.table::setnames(w, old = "COMM", new = "ACTS")
    } else {
      nd <- names(dimnames(w))
      nd[nd %in% "COMM"] <- "ACTS"
      names(dimnames(w)) <- nd
    }
    weights$ISEP <- w
  }

  flip_headers <- sub("-", "", grep("-", unlist(weight_map), value = TRUE))
  weight_map <- lapply(weight_map, gsub, pattern = "-", replacement = "")

  # GTAP-E redefines INCPAR/SUBPAR from COMM to TOPP (= the energy
  # composite eny plus the non-energy commodities), so their weights
  # cannot be reduced to the parameter's own sets. Weight them with the
  # same private consumption mapped through that correspondence: the
  # energy commodities carry the eny node, everything else is identity
  # and the by-set sum below does the collapsing. The weight headers are
  # shared with the Armington parameters, which still read them over
  # COMM, so map a copy rather than renaming in place.
  is_topp <- vapply(i_data, function(h) {
    is.data.frame(h) && "TOPP" %in% colnames(h)
  }, logical(1))
  topp_par <- intersect(names(i_data)[is_topp], names(weight_map))
  for (p in topp_par) {
    w_headers <- weight_map[[p]]
    weight_map[[p]] <- paste0(w_headers, ".TOPP")
    topp <- unique(as.character(i_data[[p]][["TOPP"]]))
    for (k in seq_along(w_headers)) {
      nm <- weight_map[[p]][[k]]
      if (is.null(weights[[nm]])) {
        weights[[nm]] <- .weight_over_topp(
          w = weights[[w_headers[[k]]]],
          topp = topp,
          e_header = w_headers[[k]]
        )
      }
    }
  }

  # several parameter headers share weight headers and set signatures;
  # reduce each (weight, kept-sets) combination only once
  reduce_cache <- new.env(parent = emptyenv())

  i_data <- lapply(i_data, function(h) {
    if (inherits(h, names(weight_map))) {
      w_headers <- weight_map[[class(h)[1]]]
      ls_w <- weights[w_headers]
      sets <- colnames(h)[!colnames(h) %in% "Value"]
      w <- data.table::rbindlist(lapply(ls_w, function(weight) {
        flip <- inherits(weight, flip_headers)
        if (!is.data.frame(weight)) {
          cache_key <- paste(c(class(weight)[1], sort(sets)), collapse = "|")
          reduced <- reduce_cache[[cache_key]]
          if (is.null(reduced)) {
            reduced <- .reduce_array(arr = weight, keep = sets)
            reduce_cache[[cache_key]] <- reduced
          }
          weight <- reduced
        }
        weight <- weight[, list(Value = sum(Value)), by = sets]
        if (flip) {
          weight[, let(Value = Value * -1)]
        }
        return(weight)
      }))[, list(omega = sum(Value)), by = sets]
      h <- merge(h, w, sets)
      h[, let(sigma = Value * omega)]
    }
    return(h)
  })

  return(i_data)
}
