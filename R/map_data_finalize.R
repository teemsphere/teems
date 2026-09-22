#' @keywords internal
#' @noRd
.finalize_map_data <- function(model,
                               sets,
                               set_raw,
                               call,
                               data_call) {
  map_rows <- model[model$type == "Mapping", ]
  if (nrow(map_rows) == 0L) {
    map_data <- list()
    return(map_data)
  }
  byele <- model$type == "Read" &
    !is.na(model$qualifier_list) &
    grepl("by_elements", model$qualifier_list, ignore.case = TRUE)
  reads <- model[byele, ]

  out <- vector("list", nrow(map_rows))
  names(out) <- character(nrow(map_rows))

  for (i in seq_len(nrow(map_rows))) {
    map_name <- map_rows$name[i]
    dom <- map_rows$comp1[i]
    cod <- map_rows$comp2[i]
    onto <- isTRUE(grepl("onto", map_rows$qualifier_list[i], ignore.case = TRUE))
    rd <- reads[tolower(reads$name) == tolower(map_name), ]
    if (nrow(rd) == 0L) {
      next
    }
    rd <- rd[1, ]
    header <- rd$header

    raw_idx <- match(toupper(header), toupper(names(set_raw)))
    if (is.na(raw_idx)) {
      .cli_action(deploy_err$map_data_missing,
        action = c("abort", "inform"),
        call = data_call
      )
    }
    vals <- set_raw[[raw_idx]]

    dom_idx <- match(tolower(dom), tolower(sets$name))
    cod_idx <- match(tolower(cod), tolower(sets$name))
    dom_map <- sets$mapping[[dom_idx]]
    cod_map <- sets$mapping[[cod_idx]]

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
  return(out[nzchar(names(out))])
}
