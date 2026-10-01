#' @keywords internal
#' @noRd
.resolve_vpqtype <- function(tab,
                             call) {
  types <- c("value", "price", "quantity", "none", "unspecified")
  kw <- tolower(sub("^\\s*([A-Za-z_]+).*$", "\\1", tab))
  stmt_rx <- "^\\s*variable\\s*\\(([^()]*)\\)\\s*$"
  defaults <- character(0)
  named <- character(0)
  declared <- list()
  drop <- logical(length(tab))
  for (i in which(kw == "variable")) {
    text <- gsub("#[^#]*#", "", tab[i])
    if (grepl(stmt_rx, text, ignore.case = TRUE)) {
      toks <- strsplit(trimws(tolower(sub(stmt_rx, "\\1", text, ignore.case = TRUE))), "\\s+")[[1]]
      vpq_stmt <- trimws(tab[i])
      if (identical(toks[1], "begins") && length(toks) == 5L &&
        setequal(toks[3:4], c("default", "vpqtype"))) {
        prefix <- toks[2]
        value <- toks[5]
        if (value == "off") {
          defaults <- defaults[names(defaults) != prefix]
        } else if (value %in% types) {
          defaults[prefix] <- value
        } else {
          vpq_value <- value
          .cli_action(model_err$vpqtype_unknown,
            action = "abort",
            call = call
          )
        }
        drop[i] <- TRUE
        next
      }
      if (identical(toks[1], "name") && length(toks) == 4L && toks[3] == "vpqtype") {
        if (!toks[4] %in% types) {
          vpq_value <- toks[4]
          .cli_action(model_err$vpqtype_unknown,
            action = "abort",
            call = call
          )
        }
        if (!is.na(named[toks[2]]) && named[toks[2]] != toks[4]) {
          vpq_var <- toks[2]
          .cli_action(model_err$vpqtype_conflict,
            action = "abort",
            call = call
          )
        }
        named[toks[2]] <- toks[4]
        drop[i] <- TRUE
        next
      }
      if (any(c("begins", "name") == toks[1]) || "vpqtype" %in% toks) {
        .cli_action(model_err$vpqtype_statement,
          action = "abort",
          call = call
        )
      }
    }
    rest <- sub("^\\s*[A-Za-z_]+", "", text)
    quals <- character(0)
    while (grepl("^\\s*\\(", rest)) {
      grp <- regmatches(rest, regexec("^\\s*\\(([^)]*)\\)", rest))[[1]]
      if (length(grp) == 0L) {
        break
      }
      if (!grepl("^\\s*all\\s*,", grp[2], ignore.case = TRUE)) {
        quals <- c(quals, trimws(strsplit(grp[2], ",", fixed = TRUE)[[1]]))
      }
      rest <- sub("^\\s*\\([^)]*\\)", "", rest)
    }
    name <- tolower(sub("^\\s*([A-Za-z_][A-Za-z0-9_@]*).*$", "\\1", rest))
    if (!nzchar(name) || identical(name, rest)) {
      next
    }
    quals <- tolower(gsub("\\s", "", quals))
    explicit <- sub("^vpqtype=", "", quals[startsWith(quals, "vpqtype=")])
    if (length(explicit) > 0L && !all(explicit %in% types)) {
      vpq_value <- explicit[!explicit %in% types][1]
      .cli_action(model_err$vpqtype_unknown,
        action = "abort",
        call = call
      )
    }
    linear <- name
    if ("levels" %in% quals) {
      linear <- paste0(if ("change" %in% quals) "c_" else "p_", name)
    }
    hit <- if (length(defaults) > 0L) names(defaults)[startsWith(linear, names(defaults))] else character(0)
    from_default <- if (length(hit) > 0L) {
      defaults[[hit[which.max(nchar(hit))]]]
    } else {
      NA_character_
    }
    declared[[name]] <- list(explicit = unique(explicit), default = from_default)
  }
  vpq <- character(0)
  for (name in names(declared)) {
    explicit <- unique(c(declared[[name]]$explicit, named[name][!is.na(named[name])]))
    if (length(explicit) > 1L) {
      vpq_var <- name
      .cli_action(model_err$vpqtype_conflict,
        action = "abort",
        call = call
      )
    }
    vpq[name] <- if (length(explicit) == 1L) {
      explicit
    } else if (!is.na(declared[[name]]$default)) {
      declared[[name]]$default
    } else {
      "unspecified"
    }
  }
  orphan <- setdiff(names(named), names(declared))
  if (length(orphan) > 0L) {
    .cli_action(model_wrn$vpqtype_orphan,
      action = "warn",
      call = call
    )
  }
  tab <- tab[!drop]
  attr(tab, "vpqtype") <- vpq
  return(tab)
}
