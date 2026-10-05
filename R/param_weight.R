#' @importFrom data.table copy setnames
#' @keywords internal
#' @noRd
.weight_over_topp <- function(w, topp, e_header) {

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

#' @keywords internal
#' @noRd
.weight_entry <- function(entry) {
  flip <- startsWith(entry, "-")
  entry <- sub("^-", "", entry)
  restrict <- regmatches(entry, regexec("^([^[]+)\\[([^=]+)=([^]]+)\\]$", entry))[[1]]
  parsed <- list(header = entry, flip = flip, dim = NA_character_, set = NA_character_)
  if (length(restrict) %=% 4L) {
    parsed[c("header", "dim", "set")] <- as.list(restrict[2:4])
  }
  return(parsed)
}

#' @keywords internal
#' @noRd
.weight_restrict <- function(weight,
                             entry,
                             sets) {
  e_header <- entry$header
  e_dim <- entry$dim
  e_set <- entry$set
  elements <- sets[[e_set]]
  dims <- if (is.data.frame(weight)) colnames(weight) else names(dimnames(weight))
  if (is.null(elements) || !e_dim %in% dims) {
    .cli_action(data_err$e_weight_set, action = "abort")
  }
  if (is.data.frame(weight)) {
    restricted <- weight[weight[[e_dim]] %in% elements]
  } else {
    idx <- lapply(dim(weight), seq_len)
    k <- match(e_dim, dims)
    idx[[k]] <- which(tolower(dimnames(weight)[[k]]) %in% elements)
    restricted <- do.call(`[`, c(list(unclass(weight)), idx, list(drop = FALSE)))
  }
  return(restricted)
}

#' @importFrom data.table rbindlist
#' @keywords internal
#' @noRd
.weight_value <- function(entries,
                          weights,
                          sets,
                          h_sets,
                          reduce_cache) {
  Value <- NULL

  w <- data.table::rbindlist(lapply(entries, \(e) {
    entry <- .weight_entry(e)
    weight <- weights[[entry$header]]
    if (!is.na(entry$dim)) {
      weight <- .weight_restrict(weight = weight, entry = entry, sets = sets)
    }
    if (!is.data.frame(weight)) {
      cache_key <- paste(c(sub("^-", "", e), sort(h_sets)), collapse = "|")
      reduced <- reduce_cache[[cache_key]]
      if (is.null(reduced)) {
        reduced <- .reduce_array(arr = weight, keep = h_sets)
        reduce_cache[[cache_key]] <- reduced
      }
      weight <- reduced
    }
    weight <- weight[, list(Value = sum(Value)), by = h_sets]
    if (entry$flip) {
      weight[, let(Value = Value * -1)]
    }
    return(weight)
  }))
  w <- w[, list(omega = sum(Value)), by = h_sets]
  return(w)
}

#' @importFrom data.table rbindlist setnames
#' @keywords internal
#' @noRd
.weight_nest <- function(nest,
                         weights,
                         sets,
                         set_mappings,
                         h_sets) {
  Value <- k <- NULL

  arrs <- lapply(nest$inputs, \(group) {
    group_arrs <- lapply(group, \(e) {
      entry <- .weight_entry(e)
      arr <- weights[[entry$header]]
      if (!is.na(entry$dim)) {
        arr <- .weight_restrict(weight = arr, entry = entry, sets = sets)
      }
      arr <- unclass(arr)
      if (!is.null(nest$dims)) {
        names(dimnames(arr)) <- nest$dims
      }
      return(arr)
    })
    return(group_arrs)
  })

  over <- nest$over %|||% NA_character_
  dims <- Reduce(intersect, lapply(unlist(arrs, recursive = FALSE), \(a) names(dimnames(a))))
  cell <- setdiff(dims, over)

  parts <- data.table::rbindlist(lapply(seq_along(arrs), \(g) {
    reduced <- data.table::rbindlist(lapply(arrs[[g]], .reduce_array, keep = dims), use.names = TRUE)
    reduced <- reduced[, list(Value = sum(Value)), by = dims]
    if (is.na(over)) {
      reduced[, let(k = as.character(g))]
    } else {
      data.table::setnames(reduced, over, "k")
    }
    return(reduced)
  }), use.names = TRUE)

  map <- nest$map %|||% over
  tab <- if (is.na(over)) NULL else set_mappings[[map]]
  if (!is.null(tab)) {
    r_idx <- match(parts$k, tolower(tab[, 1][[1]]))
    .abort_unmapped(parts$k, r_idx, map)
    parts[, let(k = tab[, 2][[1]][r_idx])]
  }

  inputs <- parts[, list(Value = sum(Value)), by = c(cell, "k")]
  nest_w <- inputs[, list(Value = sum(Value) - sum(Value^2) / sum(Value)), by = cell]
  nest_w[!is.finite(Value), let(Value = 0)]
  nest_w <- nest_w[, list(Value = sum(Value)), by = h_sets]
  return(nest_w)
}

#' @importFrom data.table rbindlist setnames
#' @keywords internal
#' @noRd
.weight_param <- function(i_data,
                          weights,
                          sets,
                          set_mappings,
                          methods,
                          data_format) {

  omega <- omega_v <- Value <- TOPP <- sigma <- NULL

  value_map <- param_weights$value[[data_format]]
  share_map <- param_weights$share[[data_format]]
  generic <- param_weights$generic[[data_format]]

  is_topp <- vapply(i_data, \(h) {
    is.data.frame(h) && "TOPP" %in% colnames(h)
  }, logical(1))
  topp_par <- intersect(names(i_data)[is_topp], names(value_map))
  for (p in topp_par) {
    w_headers <- value_map[[p]]
    value_map[[p]] <- paste0(w_headers, ".TOPP")
    topp <- unique(as.character(i_data[[p]][["TOPP"]]))
    for (k in seq_along(w_headers)) {
      nm <- value_map[[p]][[k]]
      if (is.null(weights[[nm]])) {
        weights[[nm]] <- .weight_over_topp(
          w = weights[[w_headers[[k]]]],
          topp = topp,
          e_header = w_headers[[k]]
        )
      }
    }
  }

  reduce_cache <- new.env(parent = emptyenv())

  i_data <- lapply(i_data, \(h) {
    p <- class(h)[1]
    method <- unname(methods[p])
    if (!is.data.frame(h) || is.na(method)) {
      return(h)
    }
    h_sets <- colnames(h)[!colnames(h) %in% "Value"]
    w_value <- NULL
    if (!is.null(value_map[[p]])) {
      w_value <- .weight_value(
        entries = value_map[[p]],
        weights = weights,
        sets = sets,
        h_sets = h_sets,
        reduce_cache = reduce_cache
      )
    }
    if (method %=% "share" && !is.null(share_map[[p]])) {
      w <- data.table::rbindlist(lapply(share_map[[p]], .weight_nest,
        weights = weights,
        sets = sets,
        set_mappings = set_mappings,
        h_sets = h_sets
      ))
      w <- w[, list(omega = sum(Value)), by = h_sets]
      fallback <- w_value %|||% unique(h[, h_sets, with = FALSE])[, let(omega = 1)]
      data.table::setnames(fallback, "omega", "omega_v")
      h <- merge(h, w, h_sets, all.x = TRUE)
      h <- merge(h, fallback, h_sets, all.x = TRUE)
      h[is.na(omega), let(omega = 0)]
      h[is.na(omega_v), let(omega_v = 0)]
      h[, let(sigma = Value * omega, sigma_v = Value * omega_v)]
    } else if (!is.null(w_value)) {
      h <- merge(h, w_value, h_sets)
      h[, let(sigma = Value * omega)]
      if (p %in% generic && "REG" %in% h_sets) {
        h[, let(
          Value = if (sum(omega) > 0) sum(sigma) / sum(omega) else mean(Value),
          omega = sum(omega)
        ), by = setdiff(h_sets, "REG")]
        h[, let(sigma = Value * omega)]
      }
    }
    return(h)
  })

  return(i_data)
}
