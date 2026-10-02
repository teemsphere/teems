build_model_err <- function() {
  list(
    # test-postsim.R: "forbidden statements in a PostSim section abort"
    postsim_invalid = c(
      "Statement type{?s} {.val {ps_bad_types}} {?is/are} not allowed in
      a PostSim section.",
      "PostSim sections may contain Set, Subset, Coefficient, File,
      Mapping, Read, Formula, Assertion, and Zerodivide statements
      (GEMPACK manual 12.2.1)."
    ),
    # pre-flight TAB validators (chk_tab_preflight.R); solver
    # counterparts inventoried in dev/validation_table.md
    # test-chk_tab_preflight.R: "name collisions abort"
    name_coef_var = c(
      "{cli::qty(clash)}Name{?s} declared as both a coefficient and a
      variable: {.val {clash}}.",
      "TABLO names are case-insensitive and must be unique (GEMPACK
      manual 11.2.1)."
    ),
    # test-chk_tab_preflight.R: "name collisions abort"
    name_coef_set = c(
      "{cli::qty(clash)}Name{?s} declared as both a coefficient and a
      set: {.val {clash}}.",
      "TABLO names are case-insensitive and must be unique (GEMPACK
      manual 11.2.1)."
    ),
    # test-chk_tab_preflight.R: "name collisions abort"
    name_var_set = c(
      "{cli::qty(clash)}Name{?s} declared as both a variable and a
      set: {.val {clash}}.",
      "TABLO names are case-insensitive and must be unique (GEMPACK
      manual 11.2.1)."
    ),
    # test-chk_tab_preflight.R: "duplicate declarations abort"
    name_dup = "{cli::qty(dup_names)}Duplicate {dup_type} declaration{?s}:
    {.val {dup_names}} (GEMPACK manual 11.2.1).",
    # test-chk_tab_preflight.R: "reserved words abort"
    name_reserved = "{cli::qty(res_names)}Declaration name{?s}
    {.val {res_names}} {?is a reserved word/are reserved words} (GEMPACK
    manual 11.2.1).",
    # test-chk_tab_preflight.R: "coefficients named for a levels variable's linear variable abort"
    name_c_prefix = "{cli::qty(bad_names)}Coefficient{?s} {.val {bad_names}}
    {?has/have} the name of the linear variable of a levels variable
    ({.code c_X} for a change, {.code p_X} for a percentage-change levels
    variable {.code X}; GEMPACK manual 9.2.2); rename the
    coefficient{?s}.",
    # test-chk_tab_preflight.R: "variables named for a levels variable's linear variable abort"
    name_prefix_clash = "{cli::qty(clash)}Variable{?s} {.val {clash}}
    {?has/have} the name of the linear variable of a levels variable
    ({.code c_X} for a change, {.code p_X} for a percentage-change levels
    variable {.code X}; GEMPACK manual 9.2.2); rename the
    variable{?s}.",
    # test-chk_tab_preflight.R: "over-length names abort"
    name_too_long = "{cli::qty(long_names)}Declaration name{?s} longer
    than {max_len} characters: {.val {long_names}}.",
    # test-chk_tab_preflight.R: "unknown qualifiers abort"
    qual_unknown = c(
      "{cli::qty(bad_quals)}Unknown declaration qualifier{?s}:
      {.val {bad_quals}}.",
      "See GEMPACK manual 10.3/10.4 for the recognized variable and
      coefficient qualifiers."
    ),
    # test-chk_tab_preflight.R: "no_split qualifier aborts"
    qual_no_split = "The variable qualifier {.code no_split} (full shock
    at every step) is not supported: {.val {bad_stmt}}",
    # test-chk_tab_preflight.R: "empty qualifiers abort"
    qual_empty = "Empty qualifier {.code ()} in declaration:
    {.val {bad_stmt}}",
    # test-chk_tab_preflight.R: "malformed quantifiers, sums and zerodivide defaults abort"
    quantifier_malformed = c(
      "Malformed quantifier {.val {bad_group}} in: {.val {bad_stmt}}",
      "A quantifier is {.code (all,<index>,<set>)}; the index and the set
      are both required (GEMPACK manual 10.7)."
    ),
    # test-chk_tab_preflight.R: "malformed quantifiers, sums and zerodivide defaults abort"
    dims_too_many = "{n_dims} dimensions in declaration (the solver holds
    at most {max_dims}): {.val {bad_stmt}}",
    # test-chk_tab_preflight.R: "malformed quantifiers, sums and zerodivide defaults abort"
    sum_index_empty = c(
      "Sum with an empty index in: {.val {bad_stmt}}",
      "Write {.code sum(<index>,<set>, <expression>)}."
    ),
    # test-chk_tab_preflight.R: "malformed quantifiers, sums and zerodivide defaults abort"
    ref_index_empty = c(
      "Reference {.val {bad_ref}} has an empty index in: {.val {bad_stmt}}",
      "A reference carries exactly the declared indices,
      {.code NAME(<index>, ...)} (GEMPACK manual 10.3, 11.4.10)."
    ),
    # test-chk_tab_preflight.R: "statements over the solver statement buffer abort"
    stmt_too_long = c(
      "Statement {.val {bad_stmt}} needs about {stmt_len} characters in the
      solver, over its 20000-character statement limit.",
      "Split it into shorter statements, for example through intermediate
      coefficients or variables. Equation and Update statements count 2
      extra characters per variable reference."
    ),
    # test-chk_tab_preflight.R: "malformed quantifiers, sums and zerodivide defaults abort"
    zerodivide_unknown = "Zerodivide default {.val {bad_val}} is neither a
    number nor a declared coefficient: {.val {bad_stmt}}",
    # test-chk_tab_preflight.R: "a qualifier list that never closes aborts"
    qual_unbalanced = "Unbalanced parentheses in the qualifier list of:
    {.val {bad_stmt}}",
    # test-chk_tab_preflight.R: "unbalanced parentheses abort"
    stmt_unbalanced = c(
      "Unbalanced parentheses in: {.val {bad_stmt}}",
      "Every {.code (} needs a matching {.code )}; an unclosed
      {.code sum(} is the common case."
    ),
    # test-chk_tab_preflight.R: "duplicate bounds abort"
    bound_dup = c(
      "Duplicate {bound_dir} bound in declaration: {.val {bad_stmt}}",
      "One lower ({.code ge}/{.code gt}) and one upper
      ({.code le}/{.code lt}) bound are allowed per declaration (GEMPACK
      manual 10.19.1)."
    ),
    # test-chk_tab_preflight.R: "invalid Default statements abort"
    default_homotopy = "Equation {.code (default=add_homotopy)} is not
    supported (GEMPACK manual 10.19): {.val {bad_stmt}}",
    # test-chk_tab_preflight.R: "invalid Default statements abort"
    default_bound = "Coefficient bound defaults are not supported
    (GEMPACK manual 10.19): {.val {bad_stmt}}",
    # test-chk_tab_preflight.R: "invalid Default statements abort"
    default_unknown = "Unknown {default_kw} default {.val {bad_val}}
    (GEMPACK manual 10.19): {.val {bad_stmt}}",
    # test-chk_tab_preflight.R: "invalid Default statements abort"
    default_keyword = "Default statements apply only to Coefficient,
    Variable, Formula, and Equation declarations (GEMPACK manual 10.19):
    {.val {bad_stmt}}",
    # test-ems_model.R: "unbalanced PostSim markers"
    postsim_unbalanced = "Unbalanced PostSim section markers:
    {ps_begin} {.code PostSim (Begin)} against {ps_end}
    {.code PostSim (End)} (GEMPACK manual 12.2).",
    # test-chk_tab_preflight.R: "PostSim scope violations abort"
    postsim_scope = c(
      "{cli::qty(bad_refs)}Ordinary statement{?s} reference{?s/}
      PostSim-declared name{?s}: {.val {bad_refs}}.",
      "PostSim declarations are only visible inside PostSim sections
      (GEMPACK manual 12.2.1)."
    ),
    # test-chk_tab_preflight.R: "PostSim reads from ordinary files abort"
    postsim_same_file = c(
      "{cli::qty(bad_files)}File{?s} {.val {bad_files}} read in both the
      ordinary and PostSim parts.",
      "Split the data across two files (GEMPACK manual 12.2.3)."
    ),
    # test-chk_tab_preflight.R: "PostSim reads into ordinary coefficients abort"
    postsim_read_ord = "{cli::qty(bad_targets)}PostSim Read{?s} into
    ordinary coefficient{?s} {.val {bad_targets}}; targets must be
    PostSim coefficients (GEMPACK manual 12.2.3).",
    # test-chk_tab_preflight.R: "PostSim reads into variables abort"
    postsim_read_var = "{cli::qty(bad_targets)}PostSim Read{?s} into
    variable{?s} {.val {bad_targets}}; simulation results cannot be
    changed (GEMPACK manual 12.2.3).",
    # test-chk_tab_preflight.R: "PostSim reads into undeclared names abort"
    postsim_read_undecl = "{cli::qty(bad_targets)}PostSim Read
    target{?s} {.val {bad_targets}} not declared (GEMPACK manual
    12.2.3).",
    # test-chk_tab_preflight.R: "PostSim formulas assigning variables abort"
    postsim_lhs_var = "{cli::qty(bad_lhs)}PostSim Formula{?s}
    assign{?s/} variable{?s} {.val {bad_lhs}}; simulation results cannot
    be changed (GEMPACK manual 12.2.2).",
    # test-chk_tab_preflight.R: "PostSim formulas assigning ordinary coefficients abort"
    postsim_lhs_ord = "{cli::qty(bad_lhs)}PostSim Formula{?s}
    assign{?s/} ordinary coefficient{?s} {.val {bad_lhs}}; the LHS must
    be a PostSim coefficient (GEMPACK manual 12.2.2).",
    # test-tab_levels.R: "malformed Formula & Equation aborts"
    formula_equation = "Malformed {.code Formula & Equation} statement:
    expected {.code Formula [(initial)] & Equation [(levels)] name
    [quantifiers] lhs = rhs} (GEMPACK manual 10.9.1): {.val {bad_stmt}}",
    # test-tab_levels.R: "c_-leading levels variable name aborts"
    # (p_-leading names are supported since the solver's C1a gen_lv
    # pair rename; c_-leading value references are folded into p_
    # column references by the solver preprocess and cannot be
    # distinguished)
    levels_prefix_name = c(
      "{cli::qty(bad_names)}Levels variable{?s} {.val {bad_names}}
      start{?s/} with {.code c_}, colliding with the change-reference
      column prefix; the solver cannot carry such names.",
      "Rename the {cli::qty(bad_names)}variable{?s}."
    ),
    # test-chk_tab_preflight.R: "math statements without = abort"
    stmt_missing_equals = c(
      "{stmt_kw} statement without {.code =}: {.val {bad_stmt}}",
      "Either the statement is malformed or its leading token is an
      unrecognized keyword that was read as an implicit {stmt_kw}
      continuation."
    ),
    # test-chk_statements.R: "stray label text outside a statement aborts"
    stray_label = c(
      "Statement starting with label text: {.val {bad_stmt}}",
      "A {.code # label #} outside any statement (usually a label placed
      after the terminating {.code ;}) is not valid TABLO."
    ),
    # test-chk_statements.R: "unclosed strong comment aborts (A5)"
    unclosed_strong_comment = c(
      "Strong comment {.code ![[!} opened on line {open_line} is never closed.",
      "Strong comments nest: every {.code ![[!} needs its own {.code !]]!};
      everything between the outermost pair is ignored."
    ),
    # test-chk_tab_preflight.R: "read from terminal aborts"
    read_terminal = "Read from terminal is not supported; read from a
    file instead: {.val {bad_stmt}}",
    # test-chk_tab_preflight.R: "headerless reads abort"
    read_no_header = "{cli::qty(bad_reads)}Read{?s} without a header
    {?is/are} not supported (GEMPACK manual 11.11.8): {.val {bad_reads}}",
    # test-chk_tab_preflight.R: "reads into undeclared names abort"
    read_undeclared = "{cli::qty(bad_targets)}Read target{?s}
    {.val {bad_targets}} not declared as {?a coefficient/coefficients}.",
    # Mapping statements (GEMPACK manual 11.9); solver counterparts in
    # tab_parse.c mapping machinery (teems-solver M1-M3)
    # test-tab_mapping.R: "malformed mapping declarations abort"
    map_malformed = c(
      "Malformed {.field Mapping} statement: {.val {bad_stmt}}",
      "Expected {.code Mapping [(onto)] <name> from <set> to <set>;}
      (GEMPACK manual 11.9.1)."
    ),
    # test-tab_mapping.R: "mapping with undeclared sets aborts"
    map_undeclared_set = "{cli::qty(bad_sets)}Set{?s} {.val {bad_sets}}
    in the {.field Mapping} declaration of {.val {map_name}}
    {?is/are} not declared in the model.",
    # test-tab_mapping.R: "mapping name clashes abort"
    name_map_clash = c(
      "{cli::qty(clash)}Name{?s} declared as both a mapping and a
      {clash_kind}: {.val {clash}}.",
      "TABLO names are case-insensitive and must be unique (GEMPACK
      manual 11.2.1)."
    ),
    # test-tab_mapping.R: "by_elements read of a non-mapping aborts"
    byele_nonmap = "{cli::qty(bad_targets)}{.code Read (by_elements)}
    target{?s} {.val {bad_targets}} {?is/are} not {?a declared
    mapping/declared mappings} (GEMPACK manual 11.9.3).",
    # test-tab_mapping.R: "plain read of a mapping aborts"
    map_read_plain = "{cli::qty(bad_targets)}Mapping{?s}
    {.val {bad_targets}} must be read with the
    {.code (by_elements)} qualifier (GEMPACK manual 11.9.3).",
    # test-tab_mapping.R: "mapping without a read aborts"
    map_read_missing = "{cli::qty(bad_maps)}Mapping{?s}
    {.val {bad_maps}} {?has/have} no {.code Read (by_elements)}
    statement assigning {?its/their} values.",
    # Complementarity statements (GEMPACK manual 10.17/11.14; solver
    # counterparts in tab_complementarity_transform, teems-solver C1)
    # test-tab_complementarity.R: "malformed complementarity aborts"
    comp_malformed = c(
      "Malformed {.field Complementarity} statement: {.val {bad_stmt}}",
      "Expected {.code Complementarity (variable = <levels var>,
      lower_bound/upper_bound = <levels var | parameter | constant>)
      <name> [quantifiers] <expression>;} (GEMPACK manual 10.17)."
    ),
    # test-tab_complementarity.R: "missing variable qualifier aborts"
    comp_missing_variable = "{.field Complementarity} {.val {bad_stmt}}
    needs a {.code variable =} qualifier (GEMPACK manual 11.14).",
    # test-tab_complementarity.R: "non-levels complementarity variable aborts"
    comp_not_levels = "The {.field Complementarity} variable
    {.val {comp_var}} must be a declared levels variable (GEMPACK
    manual 11.14).",
    # test-tab_complementarity.R: "missing bound aborts"
    comp_no_bound = "{.field Complementarity} {.val {comp_name}} needs
    at least one of {.code lower_bound}/{.code upper_bound} (GEMPACK
    manual 10.17).",
    # test-tab_complementarity.R: "invalid bound aborts"
    comp_bad_bound = c(
      "Invalid bound {.val {bad_bound}} in {.field Complementarity}
      {.val {comp_name}}.",
      "A bound must be a levels variable, a
      {.code Coefficient (parameter)} or a real constant (GEMPACK
      manual 10.17)."
    ),
    # test-tab_complementarity.R: "long complementarity name aborts"
    comp_name_length = "{.field Complementarity} name
    {.val {comp_name}} exceeds the 10-character limit (GEMPACK manual
    11.14/11.2.1).",
    # test-tab_complementarity.R: "quantifier count mismatch aborts"
    comp_quant_count = "{.field Complementarity} {.val {comp_name}}
    has {n_quant} quantifier{?s} but {.val {ref_name}} has {n_args}
    argument{?s} (GEMPACK manual 11.14).",
    # test-tab_complementarity.R: "condensed complementarity variable aborts"
    comp_condense = c(
      "{.val {bad_var}} cannot be {bad_action}: it is the {comp_role}
      of {.field Complementarity} {.val {comp_name}}.",
      "The complementarity variable must not be substituted out or
      backsolved (GEMPACK manual 11.14.1)."
    ),
    # test-ems_model.R: "ems_model rejects invalid variable names in backsolve"
    invalid_backsolve_var = "{.val {invalid_var}} designated for backsolving not found in the model.",
    # test-ems_model.R: "ems_model rejects invalid equation names in backsolve"
    invalid_backsolve_eq = "Equation {.val {invalid_eq}} nominated for backsolving {.val {bs_var}} not found in the model.",
    # test-ems_model.R: "ems_model rejects unresolvable backsolve entries"
    backsolve_unresolvable = c(
      "No equation {.val {conv_eq}} found to backsolve {.val {bs_var}}.",
      "Unnamed {.arg backsolve} entries resolve their defining equation by the {.field E_<variable>} convention.",
      "Name the defining equation explicitly: {.code backsolve = c({bs_var} = \"<equation>\")}."
    ),
    # test-ems_model.R: "ems_model rejects conflicting condensation actions"
    condense_conflict = "Variable{?s} {.val {conflict_var}} {?is/are} nominated for more than one backsolve.",
    # test-ems_model.R: "ems_model rejects a reused backsolve equation"
    condense_eq_reused = "Equation{?s} {.val {reused_eq}} nominated for more than one backsolve.",
    # test-ems_model.R: "backsolve rule violations" (GEMPACK manual 14.1.10)
    condense_rule = c(
      "Equation {.field {eq_name}} cannot be used to backsolve {.field {var_name}}.",
      "{rule_text}",
      "GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10."
    ),
    # .rearrange_defining_eq() cancellation, injected into
    # condense_rule's {rule_text} line
    # see model_err$condense_rule
    # test-ems_model.R: "backsolve rule violations abort (GEMPACK 14.1.10)"
    condense_cancels = paste0(
      "The occurrences of the variable cancel; no expression for it ",
      "can be obtained from this equation."
    ),
    # {parse_reason} for condense_parse
    # see model_err$condense_parse
    # test-ems_model.R: "a backsolve equation the condensation parser cannot read aborts with the reason"
    condense_parse_reason = list(
      quantifier = "unsupported equation quantifier form"
    ),
    # the linear-expression parser's failure reasons. These are raised
    # with stop() and caught in .cndns_eq_entry(), which passes
    # conditionMessage() into condense_parse's {parse_reason} slot, so
    # every one of them is user-facing. sprintf where parameterised
    # see model_err$condense_parse
    # test-tab_linear_parse.R: "the linear parser names each form it refuses", "a conditional sum carries its condition through parse, rename and serialize"
    linear_reason = list(
      product = "product of two variable-bearing expressions (nonlinear)",
      division = "division by a variable-bearing expression (nonlinear)",
      power = "power of a variable-bearing expression (nonlinear)",
      if_cond = "variable reference inside an IF condition",
      expected = "expected `%s` but found `%s`",
      unbalanced = "unbalanced parentheses in a reference",
      sum_cond_unterminated = "unterminated sum condition",
      sum_cond_empty = "empty sum condition",
      trailing = "trailing tokens starting at `%s`",
      unexpected_end = "unexpected end of expression",
      unexpected_token = "unexpected token `%s`",
      sum_index = "malformed sum index",
      sum_set = "malformed sum set",
      var_in_args = "variable reference inside the arguments of `%s`",
      unrecognized = "unrecognized characters {%s}"
    ),
    # .if_index_cond() rejections; sprintf templates, injected into
    # invalid_if_index_cond's {if_reason} line
    # see model_err$invalid_if_index_cond
    # test-ems_model.R: "index, element and mapping IF comparisons take the $POS route (manual 11.4.11)"
    if_index_reason = list(
      mixed_operands = paste0(
        "An index, quoted element or mapping expression can only be ",
        "compared with another index, mapping expression or quoted ",
        "element (GEMPACK manual 11.4.11); a data comparison takes a ",
        "coefficient reference on both sides."
      ),
      both_elements = paste0(
        "Two quoted elements compared with each other is a constant ",
        "condition (GEMPACK manual 11.4.11)."
      ),
      unrelated_sets = paste0(
        "The compared sets %s and %s are neither equal nor is one a ",
        "declared subset of the other (GEMPACK manual 11.4.11.2)."
      ),
      not_intertemporal = paste0(
        "Ordered comparisons (< <= > >=) of indices need an intertemporal ",
        "set; %s is not one, so only EQ/NE apply (GEMPACK manual 11.4.11.2)."
      )
    ),
    # the individual requirements, injected into condense_rule's
    # {rule_text} line by .check_backsolve_rules(). These are sprintf
    # templates, not cli ones: the text reaches cli already substituted,
    # which is what lets `partial_range` carry literal braces
    # see model_err$condense_rule
    # test-ems_model.R: "backsolve rule violations abort (GEMPACK 14.1.10)"
    condense_rule_text = list(
      absent = "The variable does not occur in the equation.",
      element_arg = paste0(
        "Occurrence %s: an element occurs as an argument; every argument ",
        "must be an index (requirement 1)."
      ),
      sum_index = paste0(
        "Occurrence %s: a SUM index occurs as an argument; every index ",
        "must be an equation ALL index (requirement 2)."
      ),
      unbound_arg = paste0(
        "Occurrence %s: an argument is not bound by an equation ALL ",
        "quantifier (requirement 2)."
      ),
      missing_index = paste0(
        "Equation ALL index (%s) absent from occurrence %s; every equation ",
        "ALL index must appear in each occurrence (requirement 3)."
      ),
      partial_range = paste0(
        "Occurrence %s ranges over {%s} but the variable is declared over ",
        "{%s}; every index must range over the full declared set ",
        "(requirement 4)."
      ),
      repeated_index = paste0(
        "Occurrence %s: a repeated index; all indices of one occurrence ",
        "must be different (requirement 5)."
      ),
      offset_arg = paste0(
        "Occurrence %s: an argument carries a lead/lag offset; offsets ",
        "block substitution in intertemporal models (requirement 6)."
      ),
      mixed_patterns = paste0(
        "Occurrences %s and %s have different index patterns; all ",
        "occurrences must share one pattern (requirement 7)."
      )
    ),
    # test-ems_model.R: "a backsolve equation the condensation parser cannot read aborts with the reason"
    condense_parse = c(
      "Failed to parse {.field {eq_name}} into linear terms while condensing: {parse_reason}.",
      "Statement: {.field {statement}}."
    ),
    # test-ems_model.R: "backsolved variables must be endogenous in the closure"
    condense_endo = c(
      "Backsolved {.val {bs_exo}} {cli::qty(length(bs_exo))}{?is/are} exogenous in the closure.",
      "Substituted-out variables must be endogenous; swap out of the closure or drop the backsolve."
    ),
    # test-ems_model.R: "ems_model rejects invalid coefficient arguments"
    invalid_coeff = "{.arg {nme}} is not declared in the model.",
    # test-ems_model.R: "partial read statement"
    invalid_read = "Partial {.field Read} statements are not supported.",
    # test-ems_model.R: "a value for a coefficient that is neither read nor assigned aborts"
    invalid_mod = "{.arg {nme}} is neither read in nor assigned on the LHS of a formula in the model.",
    # test-ems_model.R: "invalid numeric to a formula"
    invalid_numeric = c("Directly assigned numeric values must be length 1.",
                        "To assign heterogeneous values, use a {.code data.frame} with the appropriate set columns."),
    # test-ems_model.R: "invalid tab statement"
    invalid_state = c("teems {version} does not support {.field {inv_state}} statements.",
                      "Supported statements include: {.field {supported_state}}."),
    # probably a redundant check but if a weird unrecognized statement is found then there needs to be a way of distinguishing between an implied statement and an unrecognized statement
    # not in tests: unreachable failsafe
    unsupported_tab = "Unsupported Tablo declarations detected: {.field {unsupported}}.",
    # test-ems_model.R: "invalid intertemporal header"
    invalid_int_header = c(
      "Intertemporal {header_descr} {.val {timestep_header}} not found in loaded data.",
      "Use {.fun teems::ems_option_set} {.arg {arg_name}} to set a custom {header_descr}."
    ),
    # test-ems_model.R: "invalid read statement"
    missing_file = "Read statements missing \"from file\" detected.",
    # test-chk_tab_preflight.R: "a binary switch in a set definition aborts"
    binary_switch = c(
      "Unsupported binary switch detected in a {.field Set} definition.",
      "A set selected by a condition takes the builder form (GEMPACK manual 10.1.2), e.g. {.field Set ENDWM # mobile endowments # = (all,e,ENDW: ENDOWFLAG(e,\"mobile\") ne 0);}.",
      "Otherwise declare its elements explicitly, e.g. {.field Set ENDWM # mobile endowments # (capital,unsklab,sklab);}."
    ),
    # conditional set builders (GEMPACK manual 10.1.2; solver
    # tab_setbuilder_transform); test-ems_model.R: "conditional set builders"
    set_builder_cond = c(
      "Unsupported condition in the {.field Set} builder {.val {bad_set}}: {.val {bad_def}}.",
      "A condition compares expressions over coefficients (Read, or assigned by Formulas), mappings, {.code $POS} and sums, joined by {.code and}, {.code or} and {.code not} (GEMPACK manual 10.1.2); every name it uses must be declared."
    ),
    # test-chk_tab_preflight.R: "intertemporal set builders abort"
    set_builder_int = "{.field Set} builder {.val {bad_set}} is intertemporal; builders are supported for static sets only.",
    # test-ems_model.R: "a set builder on an undeclared/unread coefficient aborts"
    set_builder_noread = c(
      "{.field Set} builder {.val {bad_set}} conditions on {.val {cond_coef}}, which is neither Read from an input file nor assigned by a Formula.",
      "Declare the coefficient and Read or compute it before the {.field Set} statement."
    ),
    # test-chk_tab_preflight.R: "a mapping-sum set builder over a non-mapping aborts"
    set_builder_nomap = c(
      "{.field Set} builder {.val {bad_set}} sums over {.val {cond_map}}, which is not a {.field Mapping} with a {.code (by_elements)} Read.",
      "The mapping-conditional sum form needs a file-Read mapping and a file-Read summed coefficient (GEMPACK manual 10.1.2)."
    ),
    # test-ems_model.R: "intertemporal set equality"
    int_set_eq_fail = c(
      "Set equality involving an intertemporal set detected: {.field {eq_statement}}.",
      "Converting between intertemporal and non-intertemporal sets via set equality is not supported."
    ),
    # test-ems_model.R: "unparseable set definition"
    invalid_set_def = "Unparseable {.field Set} definition detected: {.field {bad_def}}.",
    # test-ems_model.R: "a Set built from an excluded coefficient aborts"
    exclude_set_dep = c(
      "{.field Set} {.val {bad_set}} depends on coefficient {.val {excl_coeff}} (header {.val {excl_header}}), which is excluded from the data by {.arg full_exclude}.",
      "The flag-header approach (GTAP {.code ENDOWFLAG}/{.code SLUG}) is not supported through the R package (the solver alone accepts it): declare the set explicitly, e.g. {.code Set ENDWM (capital, labor);}."
    ),
    # test-set_expr.R: "set products reject duplicate element names"
    set_product_dup = "{.field Set} product {.val {bad_set}} produces the duplicate element name {.val {dup_ele}} (GEMPACK manual 11.7.11); rename the factor elements.",
    # test-chk_tab_preflight.R: "self-referential set expressions abort"
    set_self_ref = c(
      "Set {.field {bad_set}} references itself in its defining
      expression: {.val {bad_def}}.",
      "Define a set from other sets and quoted elements only (GEMPACK
      manual 10.1.1.1)."
    ),
    # test-chk_tab_preflight.R: "undeclared set references abort"
    set_undeclared = c(
      "{cli::qty(bad_refs)}Set{?s} referenced before declaration in
      {.val {bad_stmt}}: {.val {bad_refs}}.",
      "Sets must be declared before they are used in a definition or
      {.field Subset} statement (GEMPACK manual 10.1)."
    ),
    # test-tab_fuzz_corpus.R: "corpus fixtures abort with their named messages" (index_not_subset fixture)
    index_not_subset = c(
      "Index {.val {bad_idx}} of {.code {bad_ref}} in {.field {bad_stmt}}
      ranges over set {.val {bad_set}}, which is not {.val {decl_set}}
      (the declared set at that argument position) or a declared subset
      of it.",
      "GEMPACK requires the relation to be declared (manual 10.1.2):
      add {.code Subset {bad_set} is subset of {decl_set};}. Without it
      the solver would address the wrong elements of {.val {decl_set}}."
    ),
    # test-chk_tab_preflight.R: "set self-equality aborts"
    set_self_eq = "Set {.field {bad_set}} is defined as equal to
    itself (GEMPACK manual 10.1.2.1).",
    # test-chk_tab_preflight.R: "malformed element ranges abort"
    set_ele_range = c(
      "Element range {.val {bad_ele}} in set {.field {bad_set}} cannot
      be expanded: {range_reason}",
      "A range names two elements with the same stem and a number at
      the end, {.code grain1 - grain4} or {.code ind008 - ind112}
      (GEMPACK manual 11.2.2)."
    ),
    # injected into set_ele_range as {range_reason};
    # test-chk_tab_preflight.R: "malformed element ranges abort"
    set_ele_range_reason = list(
      form = "it is not two element names joined by one dash.",
      stem = "the two ends do not share a stem followed by a number.",
      width = paste0(
        "a zero-padded range needs the same number of digits at both ",
        "ends."
      ),
      backwards = "it runs from a larger number to a smaller one."
    ),
    # test-chk_tab_preflight.R: "malformed element lists abort"
    set_ele_list = "Malformed element list for set
    {.field {bad_set}}: {.val {bad_def}} contains
    {empty_or_malformed} elements.",
    # test-chk_tab_preflight.R: "over-length set headers abort"
    set_header_len = "Header longer than 4 characters in the
    declaration of set {.field {bad_set}}: {.val {bad_header}}.",
    # test-int_sets.R: "empty or inverted time ranges abort"
    set_int_range = c(
      "Intertemporal set {.field {bad_set}} has {range_defect} time
      range: {.val {bad_def}} resolves to {resolved_txt}.",
      "With {n_timestep} time step{?s} the valid indices are
      {.code p[0]} through {.code p[{n_timestep - 1}]}."
    ),
    # test-int_sets.R: "malformed intertemporal terms abort"
    set_int_malformed = "Malformed intertemporal set definition for
    {.field {bad_set}}: {.val {bad_def}}.",
    # test-chk_subset_containment.R: "subset containment violations abort", "intertemporal numeric elements are checked"
    subset_not_contained = c(
      "Subset {.field {bad_sub}} is not contained in
      {.field {bad_super}}: {cli::qty(missing_ele)}element{?s}
      {.val {missing_ele}} {cli::qty(missing_ele)}{?is/are} missing
      from the superset.",
      "Check the {.field Subset} statement and the aggregation
      mappings that build both sets."
    ),
    # test-ems_model.R: "IF takes a condition and one value"
    if_args = c(
      "{.field IF} term {.field {if_term}} has more than a condition and one value.",
      "An {.field IF} takes two arguments, {.code IF[condition, value]} (GEMPACK manual 11.4.6)."
    ),
    # test-ems_model.R: "IN conditions stay single (manual 11.4.7 rule 5)"
    if_in_compound = c(
      "{.field IF} condition {.field {if_cond}} combines an {.code index IN set} test with AND, OR or NOT.",
      "An {.code IN} condition cannot be combined with AND, OR or NOT (GEMPACK manual 11.4.7 rule 5); nest the IF instead."
    ),
    # test-ems_model.R: "index, element and mapping IF comparisons"
    invalid_if_index_cond = c(
      "Unsupported {.field IF} condition detected: {.field {if_cond}}.",
      "{if_reason}"
    ),
    # test-ems_model.R: "expression IF conditions"
    if_cond_variable = c(
      "{.field IF} condition references {cli::qty(bad_vars)}variable{?s} {.val {bad_vars}}: {.field {if_statement}}.",
      "Conditions are evaluated from coefficient values only (GEMPACK manual 11.4.6/11.4.8)."
    ),
    # test-ems_model.R: "invalid set qualifier"
    invalid_set_qual = "Invalid set qualifier detected: {.field {invalid_qual}}.",
    # not in tests: no test written
    set_parse_fail = "Remnant set label detected during Tablo parsing.",
    # test-ems_model.R: "data frame input missing a set"
    injection_missing_col = c(
      "Input for {.field {nme}} is missing required columns.",
      "Required: {.field {req_col}}."
    ),
    # the following error should never be issued (full will be assigned)
    # not in tests: unreachable failsafe
    entry_type = "The following closure entries have not been classified properly: {invalid_entry}.",
    # test-ems_model.R: "closure missing exo/endo spec"
    # test-tab_vpqtype.R: "an unknown VPQ type aborts"
    vpqtype_unknown = c(
      "Unknown VPQ type {.val {vpq_value}}.",
      "A VPQ type is one of {.val Value}, {.val Price}, {.val Quantity},
      {.val None} or {.val Unspecified} (GEMPACK manual 57.2)."
    ),
    # test-tab_vpqtype.R: "conflicting VPQ types for one variable abort"
    vpqtype_conflict = "Variable {.field {vpq_var}} is given more than one VPQ
    type by its {.code VPQType=} qualifier and {.code (Name ... VPQType ...)}
    statements (GEMPACK manual 57.2).",
    # test-tab_vpqtype.R: "a malformed VPQ type statement aborts"
    vpqtype_statement = c(
      "Malformed VPQ type statement: {.code {vpq_stmt}}.",
      "The forms are {.code Variable (begins <prefix> default VPQType <type>);},
      {.code Variable (begins <prefix> VPQType default OFF);} and
      {.code Variable (Name <variable> VPQType <type>);} (GEMPACK manual 57.2)."
    ),
    # test-chk_orig_level.R: "an ORIG_LEVEL naming an undeclared coefficient aborts"
    orig_level_unknown = "{.code ORIG_LEVEL={orig_coeff}} on variable
    {.field {orig_var}} names no declared coefficient (GEMPACK manual 11.6.5).",
    # test-chk_orig_level.R: "an integer ORIG_LEVEL coefficient aborts"
    orig_level_integer = "{.code ORIG_LEVEL={orig_coeff}} on variable
    {.field {orig_var}} names an integer coefficient; it must be real
    (GEMPACK manual 11.6.5).",
    # test-chk_orig_level.R: "an ORIG_LEVEL coefficient over other sets aborts"
    orig_level_sets = c(
      "{.code ORIG_LEVEL={orig_coeff}} on variable {.field {orig_var}}:
      the coefficient ranges over {.val {coeff_sets}}, the variable over
      {.val {var_sets}}.",
      "The coefficient must range over exactly the variable's sets, in the
      same order (GEMPACK manual 11.6.5)."
    ),
    missing_specification = "The closure must contain both {.val Exogenous} and {.val Rest Endogenous} entries. The inverse approach is not supported.",
    # test-ems_model.R: "ems_model errors when invalid closure mixed entry present preswap"
    mixed_invalid = "{n_invalid_entries} closure entry element{?s} in {.field {cls_entry}} do not belong to the respective variable sets: {invalid_entries}.",
    # test-ems_model.R: "ems_model errors when duplicate closure entry present preswap"
    pre_overlap_ele = "{n_overlap} tuple{?s} for {.val {e}} in the pre-swap closure with multiple entries: {overlap}.",
    # test-ems_model.R: "ems_model errors when invalid closure pure element entry present preswap"
    ele_invalid = "The closure entry tuple {.field {cls_entry}} is invalid under the current set mapping.",
    # test-ems_model.R: "ems_model errors when invalid closure subset entry present preswap"
    subset_invalid = c("Some subsets in {.field {cls_entry}} do not belong to {.field {var_name}}.",
                       "Parent sets include: {.field {var_sets}}."),
    # test-ems_model.R: "ems_model errors dots passed without names"
    no_name_coeff = "Coefficients to modify must be passed as named pairs: {.code RDLT = 1}."
    )
}

build_model_info <- function() {
  list(
    # test-ems_model.R: "netcut proxy rewrite (roadmap 6.5 E2)"
    netcut_rewrite = c(
      "Inter-period links on element slices rewritten onto minimal intertemporal proxies: {.field {proxy_summary}}.",
      "Proxy variables (NCV*) and their linking equations (E_NCV*) appear in solve outputs."
    ),
    # test-ems_model.R: "in-TAB Substitute executes as backsolve"
    substitute_as_backsolve = c(
      "In-TAB {.field Substitute} statement{?s} for {.val {sub_var}} executed as backsolve{?s}.",
      "Backsolved values remain available in solve outputs; plain substitution is not implemented."
    ),
    # test-ems_model.R: "in-TAB Omit statements exogenize the omitted variables"
    omit_exogenous = c(
      "In-TAB {.field Omit} statement{?s} for {.val {omit_var}} applied as exogenous, unshocked closure entries.",
      "Omission removes a variable from the solved system in GEMPACK; TEEMS keeps it in the model and holds it fixed, so its values remain available in solve outputs."
    ),
    # test-ems_model.R: "ranked PostSim sets are flattened to their base set"
    ranked_set_flattened = c(
      "Ranked set definition(s) for {.val {ranked_set}} read as the base set; the ranking by {.val {rank_var}} is not preserved.",
      "Ordering of report rows is left to the composed outputs."
    ),
    # test-ems_model.R: "ignore_condense disables in-TAB condensation"
    condense_ignored = "{n_ignored} in-TAB condensation statement{?s} ignored ({.code ignore_condense = TRUE}).",
    # test-ems_model.R: "GTAPv7 condenses automatically from its in-TAB statements"
    backsolve_partitioned = c(
      "Backsolve of {.val {skip_var}} skipped: its defining equation {.val {skip_eq}} was split by the IF rewrite into {.val {skip_parts}}.",
      "A variable defined piecewise over set elements stays in the solved system."
    )
  )
}

build_model_wrn <- function() {
  list(
    # test-ems_model.R: "backsolve through a coefficient pivot synthesizes a reciprocal and warns"
    condense_pivot_zero = c(
      "Backsolving {.field {var_name}} using {.field {eq_name}} divides by the coefficient expression {.field {pivot_expr}}.",
      "Ensure this expression can never be zero; a zero value will surface as a solver error."
    ),
    # test-tab_vpqtype.R: "a Name statement for an undeclared variable warns"
    vpqtype_orphan = "{.code (Name ... VPQType ...)} for undeclared
    variable{?s} {.field {orphan}} ignored (GEMPACK manual 57.2).",
    # test-ems_model.R: "netcut inflation warning"
    netcut_inflation = c(
      "Multidimensional {.field {offenders}} referenced with a lead or lag in {.field {lag_eqs}}.",
      "Every element of a lead/lagged variable joins the dense border (netcut) of the bordered matrix methods (SBBD/DBBD/NDBBD); each non-time dimension multiplies the border size.",
      "Link periods through a minimal intertemporal proxy instead, e.g. {.code capital(REG,TIME) = qo(\"capital\",REG,TIME)}, and place the lead/lag on the proxy."
    )
  )
}
