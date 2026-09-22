#' @importFrom purrr map_chr map_lgl
#' @importFrom tibble tibble
#' @keywords internal
#' @noRd
.netcut_var_table <- function(tab) {
  is_var <- grepl("^[Vv][Aa][Rr][Ii][Aa][Bb][Ll][Ee][^A-Za-z0-9_]", tab)
  rows <- lapply(which(is_var), \(s) {
    rest <- trimws(sub("^[Vv][Aa][Rr][Ii][Aa][Bb][Ll][Ee]\\s*", "", tab[s]))
    quals <- character(0)
    quants <- list()
    repeat {
      rest <- sub("^\\s+", "", rest)
      if (!startsWith(rest, "(")) {
        break
      }
      close <- .match_bracket(rest, 1L)
      if (is.na(close)) {
        break
      }
      grp <- substr(rest, 1L, close)
      m <- regmatches(grp, regexec(
        "^\\(\\s*[Aa][Ll][Ll]\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\)$",
        grp
      ))[[1]]
      if (length(m) > 0L) {
        quants[[length(quants) + 1L]] <- c(idx = m[2], set = m[3])
      } else {
        quals <- c(quals, grp)
      }
      rest <- substring(rest, close + 1L)
    }
    m <- regmatches(rest, regexec(
      "^([A-Za-z_][A-Za-z0-9_]*)\\s*(\\(([^()]*)\\))?",
      rest
    ))[[1]]
    if (length(m) %=% 0L || m[2] %=% "") {
      return(NULL)
    }
    args <- if (m[4] %=% "") {
      character(0)
    } else {
      trimws(strsplit(m[4], ",")[[1]])
    }
    q_idx <- purrr::map_chr(quants, "idx")
    q_set <- purrr::map_chr(quants, "set")
    at <- match(tolower(args), tolower(q_idx))
    if (anyNA(at)) {
      return(NULL)
    }
    list(
      name = tolower(m[2]),
      stmt = s,
      idx = args,
      sets = q_set[at],
      quals = paste(quals[!grepl("orig_level", quals, ignore.case = TRUE)],
        collapse = ""
      )
    )
  })
  rows <- rows[!purrr::map_lgl(rows, is.null)]
  vars <- tibble::tibble(
    name = purrr::map_chr(rows, "name"),
    stmt = vapply(rows, \(r) r$stmt, integer(1)),
    idx = lapply(rows, \(r) r$idx),
    sets = lapply(rows, \(r) r$sets),
    quals = purrr::map_chr(rows, "quals")
  )
  return(vars)
}
