build_deploy_err <- function() {
  list(
    # test-ems_deploy.R: "ems_deploy errors when read-in headers not present in data"
    missing_header = "Read-in headers missing from loaded data: {.val {missing_headers}}.",
    # test-set_expr.R: "set definitions that never resolve abort by name"
    while_loop = "Construction of dependent sets has failed on: {null_sets}.",
    # conditional set builders evaluated at deploy (.eval_set_builder,
    # mirror of the solver's tab_setbuilder_transform fatals);
    # test-set_builder_eval.R: "a builder whose coefficient has no loaded data aborts"
    set_builder_data = "{.field Set} builder {.val {bad_set}}: no loaded data for its condition coefficient {.val {cond_coef}}.",
    # set builders over Formula coefficients evaluated at deploy
    # (.eval_set_builder_formula); test-set_builder_eval.R: "a formula
    # builder that cannot be evaluated names the reason"
    set_builder_eval = "{.field Set} builder {.val {bad_set}}: the condition {.val {builder_cond}} cannot be evaluated at deploy: {reason}.",
    # sprintf templates injected into set_builder_eval as {reason};
    # test-set_builder_eval.R: "a formula builder that cannot be evaluated names the reason"
    set_builder_reason = list(
      char_value = "an element name is used where a number is needed",
      unsupported = "%s is not supported in a set condition or the Formulas it depends on",
      args = "%s is used with a number of arguments that does not match its declaration",
      element = "%s is referenced at an element outside its declared sets",
      unknown = "%s is not a declared coefficient, mapping or set",
      cycle = "%s depends on itself",
      no_source = "%s is neither Read nor assigned by a Formula before the Set statement",
      no_data = "%s is Read from header \"%s\", which is not in the loaded data",
      free_index = "the condition depends on index %s, which it does not bind"
    ),
    # test-set_builder_eval.R: "a builder with the wrong number of arguments aborts"
    set_builder_args = "{.field Set} builder {.val {bad_set}}: condition coefficient {.val {cond_coef}} has {n_dims} dimension{?s} but {n_args} argument{?s} {?was/were} given.",
    # test-set_builder_eval.R: "a builder naming an element outside the aggregation aborts"
    set_builder_ele = "{.field Set} builder {.val {bad_set}}: element {.val {bad_ele}} is not in the {.field {dim_set}} dimension of {.val {cond_coef}} under the current aggregation.",
    # test-set_builder_eval.R: "a builder looping over the wrong dimension aborts"
    set_builder_dim = "{.field Set} builder {.val {bad_set}}: the loop index {.val {loop_idx}} must range over {.val {cond_coef}}'s dimension set {.field {dim_set}} exactly (source set {.field {src_set}} differs).",
    # test-set_builder_eval.R: "a mapping-sum builder without its mapping aborts"
    set_builder_mapsum = c(
      "{.field Set} builder {.val {bad_set}}: the mapping-conditional sum over {.val {cond_map}} cannot be evaluated at deploy.",
      "The sum must range over the mapping's domain set, the builder over its codomain set, and the mapping needs a {.code (by_elements)} Read whose header is in the set data (GEMPACK manual 10.1.2)."
    ),
    # test-ems_deploy.R: "ems_deploy errors when read-in headers are missing mapping"
    missing_mapping = "Some read-in model sets have no mappings: {.field {m_map}}.",
    # test-ems_deploy.R: "ems_deploy errors when timesteps provided to static model"
    nonreq_tsteps = "{.arg time_steps} provided but no intertemporal sets detected in the model. See {.fun teems::ems_data}.",
    # test-ems_deploy.R: "ems_deploy errors when timesteps not provided to a dynamic model"
    missing_tsteps = "{.arg time_steps} required for intertemporal models. See {.fun teems::ems_data}.",
    # test-ems_deploy.R: "ems_deploy errors when set-calculated number of entries does not match a finalized data header"
    data_set_mismatch = "{.field {class(dt)[1]}} has {.val {nrow(dt)}} entries; {.val {expected}} expected.",
    # test-ems_deploy.R: "an unlabelled header is dimensioned from its reading coefficient"
    unlabelled_header = c(
      "Header {.val {header}} carries no set labels and its {.val {n_values}} value{?s} (shape {file_shape}) do not fit coefficient {.field {coeff}}, declared over {decl_dims}.",
      "An unlabelled header is read positionally against the declared sets of the coefficient that reads it; label the header's dimensions or correct the declaration."
    ),
    # test-set_expr.R: "set union rejects element-level overlap with disjoint origins"
    invalid_plus = "Set operator {.code +} requires disjoint sets; overlapping elements: {.field {d}}.",
    # test-set_expr.R: "set difference rejects elements absent from the minuend"
    invalid_minus = "Set operator {.code -} may only remove elements that are present; missing: {.field {d}}.",
    # test-tab_mapping.R: "a mapping over a conflicted intersection set aborts"
    # INTERSECT itself is permissive (element-level, manual 10.1.1);
    # the origin_conflict stamp set by .eval_set_expr aborts here, at
    # the one consumer that reads origin rows
    map_origin_conflict = c(
      "The {loc} set {.field {set_name}} of mapping {.val {map_name}}
      is built by an INTERSECT whose operands disagree about the
      source composition of {cli::qty(conflict)}element{?s}
      {.val {conflict}}.",
      "The by_elements composition depends on which source elements
      aggregate into {cli::qty(conflict)}{?this/these} element{?s};
      align the aggregation mappings (or the set definitions) so both
      operands agree."
    ),
    # test-ems_deploy.R: "ems_deploy errors when aggregated inputs are incomplete"
    agg_missing_tup = "{n} tuple{?s} in the provided input file for {.val {nme}} were missing: {.field {missing}}.",
    # Mapping data build (GEMPACK manual 11.9.3); mirrors the solver
    # by_elements read fatals ahead of the deploy round-trip
    # test-tab_mapping.R: "mapping header missing from the data aborts"
    map_data_missing = c(
      "No header {.val {header}} found in the input data for mapping
      {.val {map_name}}.",
      "{.code Read (by_elements)} data must be supplied as a character
      header in the {.fun teems::ems_data} inputs."
    ),
    # test-tab_mapping.R: "mapping header count mismatch aborts"
    map_data_count = "Mapping {.val {map_name}} header {.val {header}}
    holds {.val {n_vals}} value{?s}; the domain set {.field {dom}} has
    {.val {n_dom}} element{?s} in the input data.",
    # test-tab_mapping.R: "mapping values outside the codomain abort"
    map_data_ele = "{cli::qty(bad_vals)}Mapping {.val {map_name}}
    value{?s} {.val {bad_vals}} {?is/are} not {?an element/elements}
    of the codomain set {.field {cod}}.",
    # test-tab_mapping.R: "split mapping under aggregation aborts"
    map_agg_split = c(
      "Aggregated {.field {dom}} element {.val {agg_ele}} merges
      source elements with different {.field {cod}} values:
      {.field {split_detail}}.",
      "Mapping {.val {map_name}} cannot be composed under this
      aggregation; revise the {.field {dom}} aggregation or the
      {.val {header}} data."
    ),
    # test-tab_mapping.R: "onto coverage is re-checked on the aggregated sets"
    map_onto = c(
      "{cli::qty(missing_cod)}Codomain element{?s} {.val {missing_cod}}
      of the {.code (onto)} mapping {.val {map_name}} {?is/are} not
      covered after aggregation.",
      "Every {.field {cod}} element must be the value of at least one
      {.field {dom}} element (GEMPACK manual 11.9.1)."
    ),
    # test-ems_deploy.R: "ems_deploy errors when shock_file and shock are both provided"
    shk_file_shocks = c(
      "No additional shocks are accepted if a shock file is provided."
    ),
    # test-ems_deploy.R: "write_coefficients must be a logical scalar"
    write_coefficients = "{.arg write_coefficients} must be logical of length 1."
  )
}

build_deploy_info <- function() {
  list(
    # test-set_builder_eval.R: "a builder that selects nothing yields an empty set"
    set_builder_empty = c(
      "{.field Set} builder {.val {bad_set}} selected no elements of {.field {src_set}} with {.code {builder_cond}}.",
      "The set is empty: statements over it have no tuples and sums over it are zero (GEMPACK manual 11.7.9)."
    )
  )
}
