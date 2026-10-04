#' @keywords internal
#' @noRd
.aggregate_data <- function(dt,
                            sets,
                            ...) {
  UseMethod(".aggregate_data")
}

#' @importFrom rlang is_integerish
#' @keywords internal
#' @noRd
#' @method .aggregate_data dat
#' @export
.aggregate_data.dat <- function(dt,
                                sets,
                                shock = FALSE,
                                ndigits,
                                ...) {
  Value <- NULL
  
  xval_col <- colnames(dt)[!colnames(dt) %in% "Value"]
  if (!is.null(attr(dt, "positional_dim"))) {
    return(dt)
  }
  dt <- .map_data(dt = dt, sets = sets, col = xval_col)
  if (any(duplicated(xval_col))) {
    xval_col[duplicated(xval_col)] <- paste0(xval_col[duplicated(xval_col)], ".1")
    colnames(dt)[seq_along(xval_col)] <- xval_col
  }
  dt <- dt[, list(Value = sum(Value)), keyby = xval_col]
  if (!rlang::is_integerish(dt$Value) && !shock) {
    dt[, let(Value = .round_digits(Value, ndigits))]
  } 
  return(dt)
}

#' @importFrom rlang is_integerish
#' @importFrom data.table set setkeyv
#' @keywords internal
#' @noRd
#' @method .aggregate_data par
#' @export
.aggregate_data.par <- function(dt,
                                sets,
                                ndigits,
                                ...) {
  Value <- NULL
  
  weight_col <- c("omega", "sigma", "omega_v", "sigma_v")
  xval_col <- colnames(dt)[!colnames(dt) %in% c("Value", weight_col)]
  if (!is.null(attr(dt, "positional_dim"))) {
    return(dt)
  }
  dt <- .map_data(dt = dt, sets = sets, col = xval_col)
  if (any(duplicated(xval_col))) {
    xval_col[duplicated(xval_col)] <- paste0(xval_col[duplicated(xval_col)], ".1")
    colnames(dt)[seq_along(xval_col)] <- xval_col
  }

  if (all(c("sigma", "omega") %in% colnames(dt))) {
    sum_col <- intersect(c("Value", weight_col), colnames(dt))
    dt <- dt[, c(lapply(.SD, FUN = sum), list(mean_v = mean(Value))),
      .SDcols = sum_col, by = xval_col
    ]
    dt$Value <- dt$sigma / dt$omega
    if ("omega_v" %in% colnames(dt)) {
      fallback <- !is.finite(dt$Value)
      dt$Value[fallback] <- dt$sigma_v[fallback] / dt$omega_v[fallback]
    }
    fallback <- !is.finite(dt$Value)
    dt$Value[fallback] <- dt$mean_v[fallback]
    data.table::set(dt, j = c(intersect(weight_col, colnames(dt)), "mean_v"), value = NULL)
  } else {
    sets <- setdiff(colnames(dt), "Value")
    if (sets %!=% character(0)) {
      dt <- dt[, list(Value = mean(Value)), by = sets]
    }
  }
  if (!rlang::is_integerish(dt$Value)) {
    dt[, let(Value = .round_digits(Value, ndigits))]
  } 
  if (xval_col %!=% character(0)) {
    data.table::setkeyv(dt, xval_col)
  }
  return(dt)
}

#' @keywords internal
#' @noRd
#' @method .aggregate_data DPSM
#' @export
.aggregate_data.DPSM <- function(dt,
                                 sets,
                                 ndigits,
                                 ...) {
  dt <- .aggregate_data.par(dt = dt, sets = sets, ndigits = ndigits)
  return(dt)
}

#' @keywords internal
#' @noRd
#' @method .aggregate_data GSHR
#' @export
.aggregate_data.GSHR <- function(dt,
                                 sets,
                                 ndigits,
                                 ...) {
  dt <- .aggregate_data.par(dt = dt, sets = sets, ndigits = ndigits)
  return(dt)
}

#' @importFrom data.table setkey setnames
#' @importFrom purrr pluck
#' @keywords internal
#' @noRd
#' @method .aggregate_data set
#' @export
.aggregate_data.set <- function(dt,
                                sets,
                                ...) {
  
  mapping <- NULL
  origin <- NULL

  if (inherits(dt, names(sets))) {
    r_idx <- match(dt$Value, purrr::pluck(sets, class(dt)[2], 1))
    dt[[2]] <- purrr::pluck(sets, class(dt)[2], 2)[r_idx]
    data.table::setnames(dt, new = c("origin", "mapping"))
    if (!identical(dt$origin, dt$mapping)) {
      data.table::setkey(dt, mapping, origin)
    }
    return(dt)
  }
}
