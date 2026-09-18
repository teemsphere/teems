#' Build deployed (by_elements) mapping headers
#'
#' Re-derives each mapping's by_elements header under the active
#' aggregation (GEMPACK manual 11.9.3). The raw character header pairs
#' positionally with the domain set's source elements; both legs are
#' pushed through their sets' aggregation mappings and the composition
#' must be consistent: every source element merged into an aggregated
#' domain element has to land on the same aggregated codomain element,
#' otherwise the aggregation is rejected naming the offender. `(onto)`
#' coverage is re-checked on the aggregated sets, ahead of the solver's
#' own fatal.
#'
#' @param model model tibble (Mapping + Read rows)
#' @param sets finalized set extract (`.finalize_sets`)
#' @param set_raw named list of character headers in input-file order,
#'   pre-aggregation (`.process_data`)
#'
#' @return named list of "set"-classed character vectors ready for
#'   `.ems_write.set`, keyed by header
#'
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
      # no by_elements Read: the solver values the mapping itself, from a
      # Formula (manual 10.13.1) or a (project) qualifier (10.13.2)
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

    # INTERSECT-built sets are evaluated permissively (element-level,
    # manual 10.1.1) and carry an origin_conflict stamp when their
    # operands disagreed about an element's source composition; the
    # compose below is the one consumer that reads origin rows, so the
    # ambiguity becomes fatal exactly here
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

    # source-order domain elements: file-read sets pair positionally
    # with their own raw header; TAB-defined sets carry their
    # construction order in the origin column
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
