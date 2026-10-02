#' @keywords internal
#' @noRd
.finalize_map_data <- function(model,
                               sets,
                               set_raw,
                               int_raw = list(),
                               call,
                               data_call) {
  map_rows <- model[model$type == "Mapping", ]
  if (nrow(map_rows) == 0L) {
    map_data <- list()
    return(map_data)
  }
  reads <- model[model$type == "Read" & tolower(model$name) %in% tolower(map_rows$name), ]

  out <- vector("list", nrow(reads))
  names(out) <- character(nrow(reads))
  int_headers <- character(0)

  for (i in seq_len(nrow(reads))) {
    rd <- reads[i, ]
    m <- match(tolower(rd$name), tolower(map_rows$name))
    map_name <- map_rows$name[m]
    dom <- if (is.na(rd$comp1)) map_rows$comp1[m] else rd$comp1
    cod <- map_rows$comp2[m]
    onto <- is.na(rd$comp1) &&
      isTRUE(grepl("onto", map_rows$qualifier_list[m], ignore.case = TRUE))
    byele <- !is.na(rd$qualifier_list) &&
      grepl("by_elements", rd$qualifier_list, ignore.case = TRUE)
    header <- rd$header

    dom_idx <- match(tolower(dom), tolower(sets$name))
    cod_idx <- match(tolower(cod), tolower(sets$name))
    dom_map <- sets$mapping[[dom_idx]]
    cod_map <- sets$mapping[[cod_idx]]

    if (byele) {
      raw_idx <- match(toupper(header), toupper(names(set_raw)))
      if (is.na(raw_idx)) {
        .cli_action(deploy_err$map_data_missing,
          action = c("abort", "inform"),
          call = data_call
        )
      }
      vals <- set_raw[[raw_idx]]
    } else {
      raw_idx <- match(toupper(header), toupper(names(int_raw)))
      if (is.na(raw_idx)) {
        .cli_action(deploy_err$map_data_missing_int,
          action = c("abort", "inform"),
          call = data_call
        )
      }
      cod_header <- sets$header[cod_idx]
      cod_raw_idx <- match(toupper(cod_header), toupper(names(set_raw)))
      cod_orig <- if (!is.na(cod_header) && !is.na(cod_raw_idx)) {
        set_raw[[cod_raw_idx]]
      } else {
        unique(cod_map$origin)
      }
      pos <- int_raw[[raw_idx]]
      bad_pos <- unique(pos[is.na(pos) | pos < 1 | pos > length(cod_orig) | pos != round(pos)])
      if (length(bad_pos) > 0L) {
        n_cod <- length(cod_orig)
        .cli_action(deploy_err$map_data_pos,
          action = "abort",
          call = data_call
        )
      }
      vals <- cod_orig[pos]
      int_headers <- c(int_headers, header)
    }

    for (side in c("domain", "codomain")) {
      side_map <- if (side == "domain") {
        dom_map
      } else {
        cod_map
      }
      conflict <- attr(side_map, "origin_conflict")
      if (!is.null(conflict)) {
        loc <- side
        set_name <- if (side == "domain") {
          dom
        } else {
          cod
        }
        .cli_action(deploy_err$map_origin_conflict,
          action = c("abort", "inform"),
          call = data_call
        )
      }
    }

    dom_header <- sets$header[dom_idx]
    dom_raw_idx <- match(toupper(dom_header), toupper(names(set_raw)))
    dom_orig <- if (!is.na(dom_header) && !is.na(dom_raw_idx)) {
      set_raw[[dom_raw_idx]]
    } else {
      unique(dom_map$origin)
    }

    if (length(vals) != length(dom_orig)) {
      n_vals <- length(vals)
      n_dom <- length(dom_orig)
      .cli_action(deploy_err$map_data_count,
        action = "abort",
        call = data_call
      )
    }

    agg_ele <- sets$ele[[dom_idx]]
    composed <- .compose_map_values(
      vals = vals, dom_orig = dom_orig, dom_map = dom_map, cod_map = cod_map,
      agg_ele = agg_ele, map_name = map_name, dom = dom, cod = cod,
      header = header, call = data_call
    )

    if (onto) {
      missing_cod <- setdiff(sets$ele[[cod_idx]], composed)
      if (length(missing_cod) > 0L) {
        .cli_action(deploy_err$map_onto,
          action = c("abort", "inform"),
          call = data_call
        )
      }
    }

    entry <- composed
    attr(entry, "lead") <- paste(
      length(entry),
      "Strings Length",
      max(nchar(entry)),
      "Header",
      paste0('"', header, '"'),
      "LongName",
      paste0('"', map_name, " mapping\";")
    )
    attr(entry, "file") <- rd$file
    class(entry) <- c("set", class(entry))
    out[[i]] <- entry
    names(out)[i] <- header
  }
  map_data <- out[nzchar(names(out))]
  attr(map_data, "int_headers") <- int_headers
  return(map_data)
}
