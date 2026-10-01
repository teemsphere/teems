#' @importFrom purrr pluck
#' @keywords internal
#' @noRd
.process_tablo <- function(tab_file,
                           backsolve = NULL,
                           ignore_condense = FALSE,
                           type = NULL,
                           quiet = FALSE,
                           extra_statements = NULL,
                           call) {
  tab <- .check_tab_file(
    tab_file = tab_file,
    call = call
  )
  if (length(extra_statements) > 0L) {
    tab <- c(tab, .check_statements(
      tab = paste0(extra_statements, ";", collapse = "\n"),
      call = call
    ))
  }

  tab <- .canonical_names(tab)

  tab <- .resolve_vpqtype(tab, call = call)
  vpqtype <- attr(tab, "vpqtype")

  sb_rewrite <- .rewrite_set_builders(tab)
  tab <- sb_rewrite$tab

  .chk_raw_statements(tab, call = call)

  ps_decl_names <- .postsim_decl_names(tab)

  tab <- .rewrite_sum_conditions(
    tab = tab,
    call = call
  )

  tab <- .rewrite_tab_if(
    tab = tab,
    call = call
  )

  tab <- .rewrite_tab_netcut(
    tab = tab,
    call = call
  )

  condensed <- .cndns_model(
    tab = tab,
    backsolve = backsolve,
    ignore_condense = ignore_condense,
    quiet = quiet,
    call = call
  )
  tab <- condensed$tab

  extract <- .generate_extracts(
    tab = tab,
    call = call
  )

  lowered <- .lower_tab_elements(
    tab = tab,
    extract = extract,
    call = call
  )
  tab <- lowered$tab
  extract <- lowered$extract

  var_extract <- .parse_tab_obj(
    extract = extract$model,
    obj_type = "variable",
    call = call
  )

  coeff_extract <- .parse_tab_obj(
    extract = extract$model,
    obj_type = "coefficient",
    call = call
  )

  .check_orig_level(
    var_extract = var_extract,
    coeff_extract = coeff_extract,
    call = call
  )

  if (any(grepl("\\(intertemporal\\)", purrr::pluck(extract, "set", "qualifier_list")))) {
    .check_int_headers(
      coeff_extract = coeff_extract,
      call = call
    )
  }

  math_extract <- .parse_tab_maths(
    extract = extract$model,
    call = call
  )

  .check_index_domains(
    extract = extract$model,
    set_extract = extract$set,
    call = call
  )

  .check_netcut(
    var_extract = var_extract,
    math_extract = math_extract,
    set_extract = extract$set,
    call = call
  )

  if (.o_verbose() && !quiet) {
    model_summary <- .model_summary(
      var_extract = var_extract,
      coeff_extract = coeff_extract,
      math_extract = math_extract,
      extract = extract,
      condensed = condensed
    )
  }

  read_extract <- .parse_tab_read(
    extract = extract$model,
    call = call
  )

  mapping_extract <- .parse_tab_mapping(
    extract = extract$model,
    set_names = extract$set$name,
    call = call
  )

  tab <- .assemble_tab(
    tab = tab,
    extract = extract,
    var_extract = var_extract,
    coeff_extract = coeff_extract,
    math_extract = math_extract,
    read_extract = read_extract,
    mapping_extract = mapping_extract
  )

  tab <- .flag_tab_postsim(
    tab = tab,
    ps_decl_names = ps_decl_names,
    call = call
  )

  tab <- .drop_excluded_headers(tab, call = call)

  tab <- .flag_tab_cndns(
    tab = tab,
    condensed = condensed
  )

  .check_tab_preflight(tab, call = call)

  if (.o_verbose() && !quiet) {
    attr(tab, "model_summary") <- model_summary
  }

  attr(tab, "tab_file") <- basename(tab_file)
  attr(tab, "vpqtype") <- vpqtype
  attr(tab, "omit_vars") <- condensed$omit_vars
  if (length(sb_rewrite$builders) > 0L) {
    attr(tab, "set_builders") <- sb_rewrite$builders
  }
  return(tab)
}
