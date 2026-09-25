#' @noRd
#' @keywords internal
.check_swap <- function(swap,
                        var_extract,
                        sets,
                        call) {
    return(UseMethod(".check_swap"))
}

#' @noRd
#' @keywords internal
#' @export
#' @method .check_swap list
.check_swap.list <- function(swap,
                             var_extract,
                             sets,
                             call) {
  swap <- swap[[1]]
  call <- attr(swap, "call")
  .check_swap(
    swap = swap,
    var_extract = var_extract,
    sets = sets,
    call = call
  )
}

#' @noRd
#' @keywords internal
#' @export
#' @method .check_swap character
.check_swap.character <- function(swap,
                                  var_extract,
                                  sets,
                                  call) {
  swap <- ems_swap(swap)[[1]]
  swap$var <- .levels_linear_alias(swap$var, var_extract)
  if (!swap$var %in% var_extract$name) {
    var_name <- swap$var
    .cli_action(swap_err$no_var[[1]],
      action = "abort",
      call = call
    )
  }

  .abort_cndnsd(
    var_name = swap$var,
    var_extract = var_extract,
    err = swap_err$condensed_var,
    call = call
  )

  attr(swap$var, "call") <- call
  return(swap$var)
}

#' @noRd
#' @keywords internal
#' @export
#' @method .check_swap full
.check_swap.full <- function(swap,
                             var_extract,
                             sets,
                             call) {
  call <- attr(swap, "call")
  swap$var <- .levels_linear_alias(swap$var, var_extract)
  if (!swap$var %in% var_extract$name) {
    var_name <- swap$var
    .cli_action(swap_err$no_var,
      action = c("abort", "inform"),
      call = call
    )
  }

  .abort_cndnsd(
    var_name = swap$var,
    var_extract = var_extract,
    err = swap_err$condensed_var,
    call = call
  )

  attr(swap$var, "call") <- call
  return(swap$var)
}

#' @importFrom purrr pluck pmap_chr
#' @noRd
#' @keywords internal
#' @export
#' @method .check_swap partial
.check_swap.partial <- function(swap,
                                var_extract,
                                sets,
                                call) {
  call <- attr(swap, "call")
  swap$var <- .levels_linear_alias(swap$var, var_extract)

  if (!swap$var %in% var_extract$name) {
    var_name <- swap$var
    .cli_action(swap_err$no_var,
      action = c("abort", "inform"),
      call = call
    )
  }

  .abort_cndnsd(
    var_name = swap$var,
    var_extract = var_extract,
    err = swap_err$condensed_var,
    call = call
  )

  ls_mixed <- purrr::pluck(var_extract, "ls_mixed_idx", swap$var)
  ls_upper <- purrr::pluck(var_extract, "ls_upper_idx", swap$var)
  names(swap$subset) <- .canonical_mixed(names(swap$subset), ls_mixed)

  if (!all(names(swap$subset) %in% ls_mixed)) {
    non_exist_set <- setdiff(names(swap$subset), ls_mixed)
    var_name <- swap$var
    .cli_action(
      swap_err$invalid_set,
      action = c("abort", rep("inform", 3)),
      call = call
    )
  }

  for (i in seq_along(swap$subset)) {
    comp <- swap$subset[[i]]
    nm <- names(swap$subset)[[i]]

    nm_set <- .mixed_set(nm, ls_mixed, ls_upper)
    valid_ele <- with(sets$ele, get(nm_set))
    valid_subsets <- with(sets$subsets, get(nm_set))
    valid_subsets <- valid_subsets[!grepl("^IFS[0-9]+$", valid_subsets)]

    if (length(valid_subsets) == 0L || all(is.na(valid_subsets))) {
      vs_check <- character(0)
      valid_subsets <- "*none*"
    } else {
      vs_check <- valid_subsets
    }

    comp <- .canonical_ele(comp, valid_ele, vs_check)

    if (!all(comp %in% c(valid_ele, vs_check))) {
      invalid_comp <- setdiff(comp, c(valid_ele, vs_check))
      .cli_action(swap_err$invalid_comp,
        action = c("abort", rep("inform", 3)),
        call = call
      )
    }

    if (all(comp %in% valid_ele)) {
      attr(comp, "type") <- "ele"
    } else if (all(comp %in% valid_subsets)) {
      attr(comp, "type") <- "subset"
    }

    swap$subset[[i]] <- comp
  }

  m_mixed <- setdiff(ls_mixed, names(swap$subset))
  ls_missing_sets <- as.list(m_mixed)
  names(ls_missing_sets) <- m_mixed
  swap$subset <- c(swap$subset, ls_missing_sets)
  r_idx <- match(ls_mixed, names(swap$subset))
  swap$subset <- swap$subset[r_idx]
  swap$subset <- purrr::pmap_chr(
    list(
      names(swap$subset),
      swap$subset,
      ls_upper
    ),
    \(nm, c, u) {
      if (nm %=% c) {
        u
      } else if (attr(c, "type") %=% "ele") {
        paste0("\"", c, "\"")
      } else if (attr(c, "type") %=% "subset") {
        c
      }
    }
  )

  swap <- paste0(
    swap$var,
    "(",
    paste0(swap$subset, collapse = ","),
    ")"
  )

  attr(swap, "call") <- call
  return(swap)
}
