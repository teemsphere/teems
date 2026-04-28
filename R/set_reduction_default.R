#' @importFrom data.table fsetdiff copy fsetequal `:=` setnames
#' @importFrom purrr list_flatten map2_lgl compact map_chr
#' @importFrom stats setNames
#'
#' @method .reduce2sets default
#' @export
.reduce2sets.default <- function(preswap,
                                 swap,
                                 reduced_entry,
                                 ...) {
  var_name <- attr(preswap, "var_name")
  preswap_comp <- attr(preswap, "comp")
  swap_comp <- attr(swap, "comp")

  full_dt <- data.table::copy(attr(preswap, "ele"))
  swap_ele <- data.table::copy(attr(swap, "ele"))

  orig_names <- colnames(attr(preswap, "ele"))
  unique_names <- make.unique(orig_names)
  has_dupes <- orig_names %!=% unique_names
  if (has_dupes) {
    data.table::setnames(full_dt, unique_names)
    data.table::setnames(swap_ele, unique_names)
  }
  name_map <- stats::setNames(orig_names, unique_names)

  swapped_positions <- which(swap_comp != preswap_comp)
  swapped_sets <- unique_names[swapped_positions]

  diff_dt <- data.table::fsetdiff(full_dt, swap_ele)

  if (inherits(preswap, "mixed")) {
    mixed_comp <- which(preswap_comp != orig_names)
    colnames(full_dt)[mixed_comp] <- paste0("\"", preswap_comp[mixed_comp], "\"")
    colnames(diff_dt)[mixed_comp] <- paste0("\"", preswap_comp[mixed_comp], "\"")
  }

  for (d in seq_along(swapped_sets)) {
    set_name <- swapped_sets[d]

    if (d %=% 1L) {
      full_dt <- split(full_dt, by = set_name)
      diff_dt <- split(diff_dt, by = set_name)
      diff_dt <- diff_dt[match(names(full_dt), names(diff_dt))]

      if (!all(names(full_dt) %in% names(diff_dt))) {
        null_sets <- data.table::copy(full_dt[!names(full_dt) %in% names(diff_dt)])
        lapply(null_sets, function(s) {
          cols <- names(s)
          na_values <- lapply(s, function(na) {
            switch(class(na)[1],
              "character" = NA_character_,
              "integer"   = NA_integer_,
              "numeric"   = NA_real_,
              "logical"   = NA,
              NA
            )
          })
          s[, (cols) := na_values]
        })
        diff_dt <- c(diff_dt, null_sets)
        diff_dt <- diff_dt[match(names(full_dt), names(diff_dt))]
      }
    } else {
      diff_dt <- purrr::list_flatten(lapply(diff_dt, split, by = set_name))
      full_dt <- purrr::list_flatten(lapply(full_dt, split, by = set_name))
      full_dt <- full_dt[names(full_dt) %in% names(diff_dt)]
    }

    complete <- purrr::map2_lgl(full_dt, diff_dt, data.table::fsetequal)

    rename_split_col <- function(subset) {
      if (!all(is.na(subset))) {
        val <- unlist(unique(subset[, set_name, with = FALSE]))
        replacement <- paste0("\"", val, "\"")
        colnames(subset)[colnames(subset) == set_name] <- replacement
      }
      subset
    }

    diff_dt <- lapply(diff_dt, rename_split_col)
    full_dt <- lapply(full_dt, rename_split_col)

    if (any(complete)) {
      ls_reduced <- diff_dt[complete]
      for (out in seq_along(ls_reduced)) {
        reduced_entry[[length(reduced_entry) + 1]] <- colnames(ls_reduced[[out]])
      }
    }

    diff_dt <- purrr::compact(diff_dt[!complete])
    full_dt <- purrr::compact(full_dt[!complete])
  }

  reduced_entry <- purrr::map_chr(reduced_entry, function(cols) {
    mapped <- name_map[cols]
    cols[!is.na(mapped)] <- unname(mapped[!is.na(mapped)])
    paste0(var_name, "(", paste(cols, collapse = ","), ")")
  })

  return(reduced_entry)
}