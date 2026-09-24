#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.backsolve_all <- function(tab,
                           pairs,
                           var_extract,
                           coeff_extract,
                           eq_names,
                           call) {
  extract <- .generate_extracts(tab = tab, call = call)
  math_extract <- .parse_tab_maths(
    extract = extract$model,
    call = call
  )
  math_extract <- math_extract[math_extract$type %in% "Equation", ]

  var_lookup <- as.list(stats::setNames(
    var_extract$name,
    tolower(var_extract$name)
  ))

  taken <- tolower(c(
    var_extract$name, coeff_extract$name, math_extract$name,
    extract$set$name
  ))
  csub <- new.env(parent = emptyenv())
  csub$counter <- 0L
  csub$taken <- taken
  csub$cache <- list()
  csub$statements <- character()

  eqs <- new.env(parent = emptyenv())

  for (pair in pairs) {
    def <- .eq_entry(
      eq_name = pair$eq,
      eqs = eqs,
      math_extract = math_extract,
      tab = tab,
      var_lookup = var_lookup,
      call = call
    )

    occ_check <- .check_backsolve_rules(
      var_name = pair$var,
      entry = def,
      decl_sets = var_extract$ls_upper_idx[[pair$var]],
      call = call
    )

    solution <- .rearrange_defining(
      entry = def,
      var_name = pair$var,
      def_args = occ_check$args,
      csub = csub,
      call = call
    )

    ref_pattern <- paste0("(?<![[:alnum:]_@])", pair$var, "(?![[:alnum:]_@])")
    for (e in seq_len(nrow(math_extract))) {
      eq_name <- math_extract$name[[e]]
      if (tolower(eq_name) %=% tolower(pair$eq)) {
        next
      }
      loaded <- !is.null(eqs[[tolower(eq_name)]])
      if (!loaded &&
        !grepl(ref_pattern, math_extract$definition[[e]],
          perl = TRUE, ignore.case = TRUE
        )) {
        next
      }
      entry <- .eq_entry(
        eq_name = eq_name,
        eqs = eqs,
        math_extract = math_extract,
        tab = tab,
        var_lookup = var_lookup,
        call = call
      )
      .substitute_into_eq(
        entry_name = tolower(eq_name),
        eqs = eqs,
        var_name = pair$var,
        def_args = occ_check$args,
        solution = solution,
        csub = csub
      )
    }
  }

  for (nm in ls(eqs)) {
    entry <- eqs[[nm]]
    if (!entry$dirty) {
      next
    }
    tab[[entry$row_id]] <- .serialize_eq_statement(entry)
  }

  tab <- c(tab, csub$statements)
  return(tab)
}