#' @keywords internal
#' @noRd
.stmt_target <- function(statement, keyword) {
  if (!grepl(paste0("^\\s*", keyword, "\\b"), statement, ignore.case = TRUE)) {
    return(NA_character_)
  }
  body <- sub(paste0("^\\s*", keyword, "\\s*"), "", statement, ignore.case = TRUE)
  body <- gsub("#[^#]*#", " ", body)
  repeat {
    body <- sub("^\\s+", "", body)
    if (!startsWith(body, "(")) {
      break
    }
    close <- .match_bracket(body, 1L)
    if (is.na(close)) {
      return(NA_character_)
    }
    body <- substring(body, close + 1L)
  }
  m <- regmatches(body, regexec("^([A-Za-z_@][A-Za-z0-9_@]*)", body))[[1]]
  if (length(m) == 0L) {
    return(NA_character_)
  }
  return(m[2])
}

#' @keywords internal
#' @noRd
.stmt_formula_rhs <- function(statement) {
  body <- sub("^\\s*formula\\s*", "", statement, ignore.case = TRUE)
  body <- gsub("#[^#]*#", " ", body)
  repeat {
    body <- sub("^\\s+", "", body)
    if (!startsWith(body, "(")) {
      break
    }
    close <- .match_bracket(body, 1L)
    if (is.na(close)) {
      return(NA_character_)
    }
    body <- substring(body, close + 1L)
  }
  return(body)
}

#' @keywords internal
#' @noRd
.builder_simple_ok <- function(b, read_names, formula_rows) {
  if (is.null(b)) {
    return(FALSE)
  }
  if (b$form == "mapsum" || tolower(b$coef) %in% read_names) {
    return(TRUE)
  }
  fake <- data.frame(
    type = rep("Formula", length(formula_rows)),
    tab = formula_rows,
    stringsAsFactors = FALSE
  )
  fake$definition <- as.list(vapply(formula_rows, .stmt_formula_rhs, character(1), USE.NAMES = FALSE))
  return(!is.null(.indicator_formulas(fake, b$coef)))
}

#' @keywords internal
#' @noRd
.rewrite_set_builders <- function(tab) {
  kw <- tolower(sub("^\\s*([A-Za-z]+).*$", "\\1", tab))
  set_rows <- which(kw == "set" & grepl("=\\s*\\(\\s*all\\s*,", tab, ignore.case = TRUE))
  none <- list(tab = tab, builders = list())
  if (length(set_rows) == 0L) {
    return(none)
  }
  read_rows <- tab[kw == "read"]
  read_names <- tolower(vapply(read_rows, .stmt_target, character(1), keyword = "read", USE.NAMES = FALSE))
  formula_rows <- tab[kw == "formula"]
  formula_names <- tolower(vapply(formula_rows, .stmt_target, character(1), keyword = "formula", USE.NAMES = FALSE))
  map_names <- tolower(vapply(tab[kw == "mapping"], .stmt_target, character(1), keyword = "mapping", USE.NAMES = FALSE))
  set_names <- tolower(vapply(tab[kw == "set"], .stmt_target, character(1), keyword = "set", USE.NAMES = FALSE))
  file_m <- regmatches(read_rows, regexpr("from\\s+file\\s+[A-Za-z_@][A-Za-z0-9_@]*", read_rows, ignore.case = TRUE))
  if (length(file_m) == 0L) {
    return(none)
  }
  files <- sub("^from\\s+file\\s+", "", file_m, ignore.case = TRUE)
  counts <- table(factor(files, levels = unique(files)))
  data_file <- names(counts)[which.max(counts)]
  taken_hdr <- toupper(unlist(regmatches(tab, gregexpr('(?<=header ")[^"]*', tab, perl = TRUE, ignore.case = TRUE))))
  taken_names <- tolower(unlist(regmatches(tab, gregexpr("[A-Za-z_@][A-Za-z0-9_@]*", tab))))

  builders <- list()
  insert <- list()
  k <- 0L
  for (r in set_rows) {
    st <- tab[r]
    eq <- regexpr("=\\s*\\(\\s*all\\s*,", st, ignore.case = TRUE)
    d <- substring(st, eq)
    b <- .parse_set_builder(d)
    if (.builder_simple_ok(b, read_names, formula_rows[formula_names %in% tolower(b$coef)])) {
      next
    }
    inner <- sub("^=\\s*\\(", "", sub("\\)\\s*$", "", trimws(d)))
    m <- regmatches(inner, regexec(
      "^\\s*[Aa][Ll][Ll]\\s*,\\s*([A-Za-z_@][A-Za-z0-9_@]*)\\s*,\\s*([A-Za-z_@][A-Za-z0-9_@]*)\\s*:(.*)$", inner
    ))[[1]]
    if (length(m) == 0L) {
      next
    }
    cond <- tryCatch(.sbx_parse(m[4]), error = \(e) NULL)
    if (is.null(cond)) {
      next
    }
    refs <- tolower(unique(.sbx_names(cond, m[2])))
    known <- refs %in% c(read_names, formula_names, map_names, set_names)
    if (!all(known)) {
      next
    }
    repeat {
      k <- k + 1L
      coef <- sprintf("SBI%02d", k)
      hdr <- sprintf("SB%02d", k)
      if (!tolower(coef) %in% taken_names && !hdr %in% taken_hdr) {
        break
      }
    }
    owner <- .stmt_target(st, "set")
    head <- substr(st, 1L, eq - 1L)
    insert[[as.character(r)]] <- c(
      sprintf("Coefficient (all,%s,%s) %s(%s) # indicator of set %s #", m[2], m[3], coef, m[2], owner),
      sprintf('Read %s from file %s header "%s"', coef, data_file, hdr)
    )
    tab[r] <- sprintf("%s = (all,%s,%s: %s(%s) > 0.5)", sub("\\s+$", "", head), m[2], m[3], coef, m[2])
    builders[[coef]] <- list(
      set = owner, coef = coef, header = hdr, idx = m[2], src = m[3],
      cond = trimws(m[4])
    )
  }
  if (length(builders) == 0L) {
    return(none)
  }
  out <- character(0)
  for (r in seq_along(tab)) {
    out <- c(out, insert[[as.character(r)]], tab[r])
  }
  return(list(tab = out, builders = builders))
}
