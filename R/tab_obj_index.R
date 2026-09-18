#' @importFrom purrr pluck list_flatten map map_chr map2 map2_lgl
#'
# the declaration's indices: the lower-case index letters from the
# name, the sets they range over from the quantifiers, and the mixed
# spelling the downstream writers use. The quantifier order need not
# match the name's, so the sets are permuted onto the name's order.
#' @keywords internal
#' @noRd
.index_tab_obj <- function(obj) {
  obj$lower_idx <- purrr::map_chr(obj$name, \(n) {
    if (grepl("\\(", n)) {
      paste0("(", purrr::pluck(strsplit(n, "\\("), 1, 2))
    } else {
      NA
    }
  })

  obj$ls_lower_idx <- purrr::list_flatten(purrr::map(
    obj$lower_idx,
    \(i) {
      strsplit(gsub("\\(|\\)", "", i), ",")
    }
  ))


  obj$name <- purrr::map_chr(
    obj$name,
    \(n) {
      purrr::map_chr(strsplit(n, "\\("), 1)
    }
  )

  obj$ls_upper_idx <- ifelse(obj$remainder != "",
    sapply(sapply(obj$remainder, strsplit, split = ")"), \(ss) {
      sapply(strsplit(ss, ","), "[[", 3)
    }),
    NA
  )

  order_test <- purrr::map(strsplit(obj$remainder, ")"), \(r) {
    purrr::map_chr(purrr::list_flatten(strsplit(r, ",")), 2)
  })

  if (!all(purrr::map2_lgl(
    obj$ls_lower_idx,
    order_test,
    identical
  ))) {
    s_idx <- purrr::map2(obj$ls_lower_idx, order_test, match)
    obj$ls_upper_idx <- purrr::map2(obj$ls_upper_idx,
                                    s_idx,
                                    \(upper, id) {
                                      upper[id]
                                    })
    
  }

  obj$mixed_idx <- unlist(x = purrr::map2(
    obj$ls_upper_idx,
    obj$ls_lower_idx,
    .f = \(up, low) {
      if (up %!=% NA && low %!=% NA) {
        paste(map2(up, low, \(up2, low2) {
          paste0(up2, low2)
        }), collapse = ",")
      } else {
        NA_character_
      }
    }
  ))

  obj$ls_mixed_idx <- strsplit(obj$mixed_idx, ",")
  obj$mixed_idx <- paste0("(", obj$mixed_idx, ")")
  return(obj)
}
