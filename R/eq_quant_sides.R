#' @importFrom purrr map_chr
#'
# the quantifiers as objects, the index each one binds, and the two
# sides of the equation split at its top-level "="
#' @keywords internal
#' @noRd
.eq_quant_sides <- function(groups,
                            rest) {
  quant <- lapply(groups, \(g) {
    inner <- trimws(substr(g, 2L, nchar(g) - 1L))
    m <- regmatches(inner, regexec(
      "^[Aa][Ll][Ll]\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*(:.*)?$",
      inner
    ))[[1]]
    if (length(m) %=% 0L) {
      rewritten <- list(is_quant = FALSE, text = g)
      return(rewritten)
    }
    list(
      is_quant = TRUE, text = g, idx = m[2], set = m[3],
      cond = nchar(m[4]) > 0L
    )
  })
  q_idx <- purrr::map_chr(quant, \(q) {
    if (isTRUE(q$is_quant)) {
      q$idx
    } else {
      NA_character_
    }
  })

  scan <- .tab_scan(rest)
  eq_pos <- which(scan$chs == "=" & scan$depth_before == 0L & !scan$in_quote)
  sides <- list(
    trimws(substr(rest, 1L, eq_pos[1] - 1L)),
    trimws(substring(rest, eq_pos[1] + 1L))
  )
  context <- list(quant = quant, q_idx = q_idx, sides = sides)
  return(context)
}
