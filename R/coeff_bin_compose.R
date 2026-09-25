#' @importFrom purrr map map2 pmap pmap_lgl
#' @importFrom data.table data.table CJ setnames setkeyv set
#' @importFrom tibble tibble
#' @keywords internal
#' @noRd
.compose_coeff_bin <- function(data_dt,
                               coeff_extract,
                               cofs,
                               sets,
                               time_steps,
                               call) {

  cofs <- cofs[order(cofs$postsim, cofs$cofname), ]

  cofs$setid <- strsplit(cofs$setid, split = ",")
  cofs$column_id <- purrr::map2(
    cofs$setid,
    cofs$size,
    \(x, y) {
      if (y != 0) {
        x[1:y]
      } else {
        NA
      }
    }
  )

  set_tbl <- tibble::tibble(
    id = seq(0, length(sets) - 1),
    sets = sets
  )

  cofs$set <- purrr::map(cofs$column_id, \(c_id) {
    set_tbl$sets[match(c_id, set_tbl$id)]
  })

  cofs$dt <- lapply(cofs$set, \(ele) {
    if (is.null(unlist(ele))) {
      data.table::data.table(null_set = NA)
    } else {
      do.call(data.table::CJ, c(ele, sorted = FALSE))
    }
  })

  if (!all(lapply(cofs$dt, nrow) == cofs$matsize) ||
    sum(unlist(lapply(cofs$dt, nrow))) %!=% nrow(data_dt)) {
    .cli_action(compose_err$idx_mismatch,
      action = "abort",
      .internal = TRUE,
      call = call
    )
  }

  ce_idx <- match(cofs$cofname, tolower(coeff_extract$name))
  cofs <- cofs[!is.na(ce_idx), ]
  ce_idx <- ce_idx[!is.na(ce_idx)]
  coeff_extract <- coeff_extract[ce_idx, ]

  offsets <- purrr::map2(cofs$pack_begadd, cofs$matsize, \(b, m) {
    seq.int(b + 1L, length.out = m)
  })

  set_names <- names(sets)
  strict_check <- all(purrr::pmap_lgl(
    list(coeff_extract$ls_mixed_idx, coeff_extract$ls_upper_idx, cofs$column_id),
    \(mixed, upper, c_id) {
      if (mixed %=% NA_character_ || anyNA(c_id)) {
        return(mixed %=% NA_character_ && anyNA(c_id))
      }
      declared <- tolower(upper)
      dumped <- tolower(set_names[as.integer(c_id) + 1L])
      length(declared) == length(dumped) && all(declared == dumped)
    }
  ))
  if (!strict_check) {
    .cli_action(compose_err$strict_check,
      action = "abort",
      .internal = TRUE,
      call = call
    )
  }

  dats <- purrr::pmap(
    list(cofs$dt, offsets, coeff_extract$ls_mixed_idx),
    \(dt, idx, mixed) {
      if (mixed %=% NA_character_) {
        coeff_dt <- data.table::data.table(Value = data_dt$Value[idx])
        return(coeff_dt)
      }
      data.table::set(dt, j = "Value", value = data_dt$Value[idx])
      data.table::setnames(dt, new = c(mixed, "Value"))
      data.table::setkeyv(dt, cols = mixed)
      dt
    }
  )
  names(dats) <- coeff_extract$name

  if (!is.null(time_steps)) {
    dats <- lapply(dats,
      FUN = .match_year,
      sets = sets,
      time_steps = time_steps
    )
  }

  coeff_dt <- tibble::tibble(
    name = coeff_extract$name,
    label = coeff_extract$label,
    type = ifelse(cofs$postsim, "postsim", "coefficient"),
    dat = dats
  )
  return(coeff_dt)
}
