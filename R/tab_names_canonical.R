#' @keywords internal
#' @noRd
.declared_names <- function(statements) {
  decl <- regmatches(
    statements,
    regexec(
      "^\\s*(set|coefficient|variable|file|mapping|equation)\\s*((?:\\([^()]*(?:\\([^()]*\\)[^()]*)*\\)\\s*)*)([A-Za-z_@][A-Za-z0-9_@]*)",
      statements,
      ignore.case = TRUE,
      perl = TRUE
    )
  )
  names_found <- vapply(decl, \(m) {
    if (length(m) == 0L) NA_character_ else m[4]
  }, character(1))
  names_found <- names_found[!is.na(names_found)]
  names_found <- names_found[!tolower(names_found) %in% c("all", "from", "to")]
  spellings <- unique(names_found)
  clash <- unique(tolower(spellings[duplicated(tolower(spellings))]))
  names_found <- names_found[!tolower(names_found) %in% clash]
  canon <- names_found[!duplicated(tolower(names_found))]
  names(canon) <- tolower(canon)
  return(canon)
}

#' @keywords internal
#' @noRd
.canonical_segment <- function(seg, canon, skip) {
  loc <- gregexpr("(?<![A-Za-z0-9_@$])[A-Za-z_@][A-Za-z0-9_@]*", seg, perl = TRUE)
  tokens <- regmatches(seg, loc)[[1]]
  if (length(tokens) == 0L) {
    return(seg)
  }
  key <- tolower(tokens)
  hit <- canon[key]
  swap <- !is.na(hit) & hit != tokens & !key %in% skip
  if (!any(swap)) {
    return(seg)
  }
  tokens[swap] <- hit[swap]
  regmatches(seg, loc) <- list(tokens)
  return(seg)
}

#' @keywords internal
#' @noRd
.canonical_names <- function(statements) {
  canon <- .declared_names(statements)
  if (length(canon) == 0L) {
    return(statements)
  }
  protect <- "\"[^\"]*\"|#[^#]*#"
  set_list <- "\\((?!\\s*all\\s*,)[^()]*\\)"
  out <- vapply(statements, \(st) {
    idx <- regmatches(st, gregexpr(
      "(?:\\(all\\s*,\\s*|(?:sum|prod|maxs|mins)\\s*[{(]\\s*)([A-Za-z_@][A-Za-z0-9_@]*)\\s*,",
      st,
      ignore.case = TRUE,
      perl = TRUE
    ))[[1]]
    skip <- tolower(sub("^.*?([A-Za-z_@][A-Za-z0-9_@]*)\\s*,$", "\\1", sub("^\\(all\\s*,\\s*", "", idx, ignore.case = TRUE), perl = TRUE))
    pattern <- if (grepl("^\\s*set\\b", st, ignore.case = TRUE, perl = TRUE)) {
      paste0(protect, "|", set_list)
    } else {
      protect
    }
    keep <- gregexpr(pattern, st, perl = TRUE)[[1]]
    if (keep[1] == -1L) {
      segment <- .canonical_segment(st, canon, skip)
      return(segment)
    }
    starts <- as.integer(keep)
    ends <- starts + attr(keep, "match.length") - 1L
    pieces <- character(0)
    pos <- 1L
    for (k in seq_along(starts)) {
      if (starts[k] > pos) {
        pieces <- c(pieces, .canonical_segment(substr(st, pos, starts[k] - 1L), canon, skip))
      }
      pieces <- c(pieces, substr(st, starts[k], ends[k]))
      pos <- ends[k] + 1L
    }
    if (pos <= nchar(st)) {
      pieces <- c(pieces, .canonical_segment(substr(st, pos, nchar(st)), canon, skip))
    }
    paste(pieces, collapse = "")
  }, character(1), USE.NAMES = FALSE)
  return(out)
}
