#' @importFrom rlang is_integerish
#' @importFrom data.table data.table setnames setkeyv setattr
#' @importFrom purrr map2 map_chr
#' 
#' @keywords internal
#' @noRd
.finalize_data <- function(data,
                           sets,
                           model,
                           write_dir,
                           call) {

  model_coeff <- model[model$type == "Coefficient" & !is.na(model$file), "header"][[1]]
  data <- data[names(data) %in% model_coeff]

  if (attr(sets, "intertemporal")) {
    int_sets <- sets[grepl("\\(intertemporal\\)", sets$qualifier_list), "ele"][[1]]

    l_idx <- match(names(data), model$header)
    data <- purrr::map2(
      data,
      l_idx,
      function(dt, id) {
        full_sets <- model$ls_upper_idx[[id]]
        if (any(duplicated(full_sets))) {
          full_sets[duplicated(full_sets)] <- attr(dt, "sorted")[which(duplicated(full_sets))]
        }
        if (any(names(int_sets) %in% full_sets)) {
          int_set <- intersect(names(int_sets), full_sets)
          nonint_set <- full_sets[!full_sets %in% int_set]
          dt <- dt[, .(with(int_sets, get(int_set)), Value), by = nonint_set]
          data.table::setnames(dt, new = c(full_sets, "Value"))
          data.table::setkeyv(dt, cols = full_sets)
        }
        return(dt)
      }
    )

    n_timestep_header <- .o_n_timestep_header()
    timestep_header <- .o_timestep_header()
    n_timestep <- data.table::data.table(Value = length(attr(sets, "time_steps")))
    class(n_timestep) <- c(n_timestep_header, "dat", class(n_timestep))

    timestep_coeff <- model$name[match(timestep_header, model$header)]
    t_header_set <- purrr::pluck(model, "ls_upper_idx", timestep_coeff)
    timesteps <- data.table::data.table(
      with(sets$ele, get(t_header_set)),
      attr(sets, "time_steps")
    )
    class(timesteps) <- c(timestep_header, "par", class(timesteps))
    data.table::setnames(timesteps, c(t_header_set, "Value"))
    data.table::setkeyv(timesteps, cols = t_header_set)
    append <- list(n_timestep, timesteps)
    names(append) <- purrr::map_chr(append, function(c) {
      class(c)[[1]]
    })
    data <- c(data, append)
  }

  data <- lapply(data, function(dt) {
    if (!colnames(dt) %=% "Value") {
      dt_col <- gsub("\\.[0-9]+", "", colnames(dt))
      dt_sets <- with(sets$ele, mget(dt_col[!dt_col %in% "Value"]))
      expected <- as.integer(prod(lengths(dt_sets)))
      data.table::setattr(dt, "dim_sizes", lengths(dt_sets))
      data.table::setattr(dt, "dimen", paste(lengths(dt_sets), collapse = " "))
    } else {
      expected <- 1L
      data.table::setattr(dt, "dim_sizes", expected)
      data.table::setattr(dt, "dimen", as.character(expected))
    }

    if (!expected %=% nrow(dt)) {
      .cli_action(data_err$data_set_mismatch,
        action = "abort",
        call = call
      )
    }
    return(dt)
  })

  r_idx <- match(names(data), model$header)
  ndigits <- .o_ndigits()

  data <- purrr::map2(
    data,
    r_idx,
    function(dt, id) {
      data.table::setattr(dt, "file", model$file[id])
      if (!grepl("integer", model$qualifier_list[id])) {
        type <- "Real"
      } else {
        type <- "Integer"
      }

      if (type %=% "Real" && !rlang::is_integerish(dt$Value)) {
        dt[, Value := format(
          round(Value, ndigits),
          trim = TRUE,
          nsmall = ndigits,
          scientific = FALSE
        )]
      } else {
        dt[, Value := as.integer(Value)]
      }

      lead <- paste(
        attr(dt, "dimen"),
        type,
        "SpreadSheet Header",
        paste0('"', class(dt)[1], '"'),
        "LongName",
        paste0('"', model$label[id], '";')
      )

      data.table::setattr(dt, "lead", lead)
      return(dt)
    }
  )

  model_sets <- model[model$type == "Set" & !is.na(model$file), "name"][[1]]
  sets <- sets[sets$name %in% model_sets, "ele"][[1]]

  r_idx <- match(names(sets), model$name)
  sets <- purrr::map2(
    sets,
    r_idx,
    function(s, id) {
      lead <- paste(
        length(s),
        "Strings Length",
        max(purrr::map_int(s, nchar)),
        "Header",
        paste0("\"", model$header[id], "\""),
        "LongName",
        paste0('"', model$label[id], '";')
      )

      attr(s, "lead") <- lead
      attr(s, "file") <- model$file[id]
      class(s) <- c("set", class(s))
      return(s)
    }
  )

  data <- c(data, sets)
  return(data)
}