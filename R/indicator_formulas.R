#' @importFrom purrr map_lgl
#' @keywords internal
#' @noRd
.indicator_formulas <- function(model, coef) {
  id <- "[A-Za-z_][A-Za-z0-9_]*"
  rows <- which(model$type == "Formula" & purrr::map_lgl(model$definition, \(d) {
    length(d) == 1L && !is.na(d) &&
      grepl(paste0("^\\s*", coef, "\\s*[[({]"), d, ignore.case = TRUE)
  }))
  if (length(rows) == 0L) {
    return(NULL)
  }
  num <- "[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?"
  steps <- vector("list", length(rows))
  for (k in seq_along(rows)) {
    st <- model$tab[rows[k]]
    if (is.na(st)) {
      return(NULL)
    }
    body <- sub("^\\s*[Ff][Oo][Rr][Mm][Uu][Ll][Aa]\\s*", "", st)
    body <- gsub("#[^#]*#", " ", body)
    quants <- character(0)
    repeat {
      body <- sub("^\\s+", "", body)
      if (!startsWith(body, "(")) {
        break
      }
      close <- .match_bracket(body, 1L)
      if (is.na(close)) {
        return(NULL)
      }
      g <- substr(body, 1L, close)
      if (grepl("^\\(\\s*[Aa][Ll][Ll]\\s*,", g)) {
        quants <- c(quants, g)
      }
      body <- substring(body, close + 1L)
    }
    if (length(quants) != 1L) {
      return(NULL)
    }
    q <- regmatches(quants, regexec(
      paste0("^\\(\\s*[Aa][Ll][Ll]\\s*,\\s*(", id, ")\\s*,\\s*(", id, ")\\s*\\)$"), quants
    ))[[1]]
    if (length(q) == 0L) {
      return(NULL)
    }
    idx <- q[2]
    set <- q[3]
    body <- gsub("\\s", "", sub(";\\s*$", "", body))
    body <- gsub("[][]", "", body)
    self <- paste0(coef, "(", idx, ")")
    m <- regmatches(body, regexec(
      paste0("^", coef, "\\(", idx, "\\)=(", num, ")$"), body, ignore.case = TRUE
    ))[[1]]
    if (length(m) > 0L) {
      steps[[k]] <- list(set = set, idx = idx, mode = "set", value = as.numeric(m[2]))
      next
    }
    m <- regmatches(body, regexec(
      paste0("^", coef, "\\(", idx, "\\)=", coef, "\\(", idx, "\\)([-+])(", num, ")$"), body, ignore.case = TRUE
    ))[[1]]
    if (length(m) > 0L) {
      v <- as.numeric(m[3])
      steps[[k]] <- list(set = set, idx = idx, mode = "add", value = if (m[2] == "-") {
        -v
      } else {
        v
      })
      next
    }
    return(NULL)
  }
  return(steps)
}
