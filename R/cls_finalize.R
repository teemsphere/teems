#' @keywords internal
#' @noRd
.finalize_closure <- function(closure,
                              closure_file,
                              swap_in,
                              swap_out,
                              sets,
                              var_extract,
                              call,
                              model_call) {

  closure <- .validate_closure(
    closure = closure,
    sets = sets,
    var_extract = var_extract,
    call = call,
    model_call = model_call
  )

  swaps <- c(
    .prep_swaps(swap_in, "in", sets, var_extract, call),
    .prep_swaps(swap_out, "out", sets, var_extract, call)
  )

  if (length(swaps) %!=% 0L) {
    swap_keys <- lapply(swaps, .swap_keys)
    initial <- closure
    parity <- .swap_parity(swaps, swap_keys, initial)
    pending <- .swap_cancel(swaps, swap_keys, parity)
    while (length(pending) %!=% 0L) {
      ready <- 0L
      for (p in seq_along(pending)) {
        s <- pending[p]
        if (.swap_valid(swaps[[s]], swap_keys[[s]], closure)) {
          ready <- p
          break
        }
      }
      if (ready %=% 0L) {
        repeated <- parity$tuples[parity$n_in + parity$n_out > 1L]
        if (length(pending) %!=% 1L || any(parity$bad %in% repeated)) {
          .swap_unordered(
            swaps = swaps,
            swap_keys = swap_keys,
            pending = pending,
            parity = parity,
            closure = closure,
            call = call
          )
        }
        ready <- 1L
      }
      closure <- .apply_swap(
        swap = swaps[[pending[ready]]],
        closure = closure,
        sets = sets,
        var_extract = var_extract,
        call = call
      )
      pending <- pending[-ready]
    }
  }

  attr(closure, "file") <- closure_file
  class(closure) <- c("closure", class(closure))
  return(closure)
}

#' @keywords internal
#' @noRd
.prep_swaps <- function(swaps,
                        direction,
                        sets,
                        var_extract,
                        call) {
  if (is.null(swaps)) {
    return(list())
  }
  swaps <- .classify_cls(
    closure = swaps,
    sets = sets,
    call = call
  )
  swaps <- lapply(swaps,
    .exp_cls_entry,
    var_extract = var_extract,
    sets = sets$ele,
    call = call
  )
  lapply(swaps, \(s) {
    attr(s, "direction") <- direction
    s
  })
}

#' @keywords internal
#' @noRd
.swap_keys <- function(entry) {
  var_name <- attr(entry, "var_name")
  ele <- attr(entry, "ele")
  if (!is.data.frame(ele)) {
    return(var_name)
  }
  quoted <- lapply(ele, \(x) if (is.character(x)) paste0("\"", x, "\"") else x)
  paste0(var_name, "(", do.call(paste, c(unname(quoted), sep = ",")), ")")
}

#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.exo_keys <- function(closure,
                      var_name) {
  entries <- closure[purrr::map_chr(closure, attr, "var_name") == var_name]
  unlist(lapply(entries, .swap_keys))
}

#' @keywords internal
#' @noRd
.swap_valid <- function(swap,
                        keys,
                        closure) {
  exo <- .exo_keys(closure, attr(swap, "var_name"))
  if (attr(swap, "direction") %=% "in") {
    return(!any(keys %in% exo))
  }
  length(exo) %!=% 0L && all(keys %in% exo)
}

#' @keywords internal
#' @noRd
.swap_label <- function(swap) {
  if (is.null(attr(swap, "call"))) {
    return(as.character(swap))
  }
  paste(deparse(attr(swap, "call")), collapse = "")
}

#' @keywords internal
#' @noRd
.swap_tuple_list <- function(keys) {
  if (length(keys) > 5L) {
    return(sprintf(swap_err$no_order_more, paste(keys[1:5], collapse = ", "), length(keys) - 5L))
  }
  paste(keys, collapse = ", ")
}

#' @keywords internal
#' @noRd
.swap_parity <- function(swaps,
                         swap_keys,
                         initial) {
  mentions <- do.call(rbind, lapply(seq_along(swaps), \(s) {
    data.frame(
      key = swap_keys[[s]],
      var_name = attr(swaps[[s]], "var_name"),
      direction = attr(swaps[[s]], "direction"),
      swap = s
    )
  }))
  initial_exo <- unlist(lapply(unique(mentions$var_name), \(v) .exo_keys(initial, v)))
  tuples <- unique(mentions$key)
  n_in <- vapply(tuples, \(k) sum(mentions$key == k & mentions$direction == "in"), integer(1))
  n_out <- vapply(tuples, \(k) sum(mentions$key == k & mentions$direction == "out"), integer(1))
  exo <- tuples %in% initial_exo
  net <- ifelse(exo, n_out - n_in, n_in - n_out)
  list(
    mentions = mentions,
    tuples = tuples,
    n_in = unname(n_in),
    n_out = unname(n_out),
    exo = exo,
    bad = tuples[!net %in% c(0L, 1L)]
  )
}

#' @keywords internal
#' @noRd
.swap_cancel <- function(swaps,
                         swap_keys,
                         parity) {
  pending <- seq_along(swaps)
  twice <- parity$tuples[parity$n_in == 1L & parity$n_out == 1L]
  uniform <- \(keys) {
    exo <- parity$exo[match(keys, parity$tuples)]
    all(exo) || !any(exo)
  }
  for (s in pending) {
    if (!attr(swaps[[s]], "direction") %=% "out" || !all(swap_keys[[s]] %in% twice)) {
      next
    }
    for (t in pending) {
      if (attr(swaps[[t]], "direction") %=% "in" &&
        attr(swaps[[t]], "var_name") %=% attr(swaps[[s]], "var_name") &&
        setequal(swap_keys[[t]], swap_keys[[s]]) &&
        uniform(swap_keys[[s]])) {
        pending <- setdiff(pending, c(s, t))
        break
      }
    }
  }
  pending
}

#' @keywords internal
#' @noRd
.swap_unordered <- function(swaps,
                            swap_keys,
                            pending,
                            parity,
                            closure,
                            call) {
  bad <- match(parity$bad, parity$tuples)
  calls <- vapply(bad, \(t) {
    mentioned <- unique(parity$mentions$swap[parity$mentions$key == parity$tuples[t]])
    paste(vapply(swaps[mentioned], .swap_label, character(1)), collapse = "; ")
  }, character(1))
  groups <- split(bad, paste(parity$n_in[bad], parity$n_out[bad], parity$exo[bad], calls))
  parity_lines <- vapply(unname(groups), \(g) {
    t <- g[1]
    sprintf(swap_err$no_order_parity,
      .swap_tuple_list(parity$tuples[g]),
      parity$n_in[t],
      parity$n_out[t],
      if (parity$exo[t]) swap_err$no_order_status$exo else swap_err$no_order_status$endo,
      calls[match(t, bad)]
    )
  }, character(1))

  blocked_lines <- vapply(pending, \(s) {
    exo_now <- .exo_keys(closure, attr(swaps[[s]], "var_name"))
    keys <- swap_keys[[s]]
    if (attr(swaps[[s]], "direction") %=% "in") {
      blocking <- keys[keys %in% exo_now]
      template <- swap_err$no_order_blocked_in
    } else {
      blocking <- keys[!keys %in% exo_now]
      template <- swap_err$no_order_blocked_out
    }
    sprintf(template, .swap_label(swaps[[s]]), .swap_tuple_list(blocking))
  }, character(1))

  lines <- gsub("}", "}}", gsub("{", "{{", c(parity_lines, blocked_lines), fixed = TRUE), fixed = TRUE)
  n_pending <- length(pending)
  .cli_action(c(swap_err$no_order, lines),
    action = c("abort", rep("inform", length(lines))),
    call = call
  )
}

#' @importFrom purrr map map_chr pluck
#' @importFrom data.table rbindlist fintersect setnames copy
#' @importFrom utils capture.output
#' @keywords internal
#' @noRd
.apply_swap <- function(swap,
                        closure,
                        sets,
                        var_extract,
                        call) {
  var_name <- attr(swap, "var_name")
  var_entries <- closure[purrr::map_chr(closure, attr, "var_name") == var_name]

  if (attr(swap, "direction") %=% "in") {
    if (length(var_entries) %!=% 0L) {
      check <- data.table::rbindlist(purrr::map(var_entries, attr, "ele"))
      idx_sets <- purrr::pluck(var_extract, "ls_mixed_idx", var_name)
      data.table::setnames(check, new = idx_sets)
      swap_check <- data.table::copy(attr(swap, "ele"))
      data.table::setnames(swap_check, new = idx_sets)
      if (nrow(data.table::fintersect(swap_check, check)) %!=% 0L) {
        if (!is.null(attr(swap, "call"))) {
          call <- attr(swap, "call")
        }
        overlap <- data.table::fintersect(swap_check, check)
        overlap <- utils::capture.output(print(overlap))[-c(1, 2)]
        .cli_action(swap_err$overlap_ele,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
    attr(swap, "direction") <- NULL
    return(c(closure, list(swap)))
  }

  if (length(var_entries) %=% 0L) {
    if (!is.null(attr(swap, "call"))) {
      call <- attr(swap, "call")
    }
    .cli_action(swap_err$no_var_cls,
      action = c("abort", "inform"),
      call = call
    )
  }

  attr(swap, "direction") <- NULL
  .swap_out(
    swap = swap,
    closure = closure,
    sets = sets,
    var_name = var_name,
    var_extract = var_extract,
    var_entries = var_entries,
    call = call
  )
}
