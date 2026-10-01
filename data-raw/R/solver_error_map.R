# Maps solver diagnostic-log "Error:" lines to abort classes.
# Patterns are regexes matched (case-insensitively) against each
# "Error:" line with the prefix stripped; the FIRST matching row wins,
# so closure/shock and data patterns that overlap the generic TAB
# wording sit above the TAB block. Sources: the solver fatal-error
# format strings in teems-solver src (tab_parse.c, cmf_io.c,
# formula.c, jacobian.c, main.c); the row inventory lives in
# dev/validation_table.md. The manual column is the GEMPACK manual
# section cited by the solver message, NA when none.
build_solver_error_map <- function() {
  rows <- list(
    # the image does not accept what teems sent (strict options and
    # manifest, main.c cli_options_check / tab_parse.c manifest_check):
    # above every other row, since the offending text is quoted verbatim
    c("unknown command-line option", "interface", NA),
    c("of the manifest \\(\\.cmf\\) file", "interface", NA),
    c("more than one subtotals statement", "interface", NA),
    # a misspelt TAB keyword (cmf_io.c kwless_unknown), also quoting
    # the statement
    c("unknown statement keyword", "tab", "11.1.1"),
    # the -jacdump switch (main.c): a data program has no system to
    # export; any other value than 0/1 is an interface mismatch
    c("needs a simulation, and this run has none", "tab", "5.1.2"),
    c("-jacdump must be 0 \\(off\\)", "interface", NA),
    # shock groups for subtotals (tab_parse.c subtotals_read, main.c
    # method checks, solve_drivers.c extra solves): above the closure
    # and data rows, whose "is not in set"/"cannot open" wording the
    # subtotals-file messages share
    c("subtotals file", "subtotal", "29.1"),
    c("subtotals \\(manual 29\\) are not available", "subtotal", "29"),
    c("cannot keep (its factorization|yet)", "subtotal", NA),
    c("subtotal solves found no kept factorization", "subtotal", NA),
    c("kept (SBBD|DBBD) factor", "subtotal", NA),
    # closure / shock files (closure_read wording: "(in <var>)";
    # shocks_read wording: "(shock file)")
    c("is not in set .* \\(in ", "closure", NA),
    c("is not declared \\(in ", "closure", NA),
    c("which is endogenous; only exogenous components", "closure", "24.14.1"),
    c("specified more than once", "closure", "68.1.1"),
    c("has no exogenous components, so it cannot be shocked", "closure", "24.6.3"),
    c("exogenous of [0-9]+ components", "closure", "24.14.1"),
    c("shock file", "closure", NA),
    c("closure file", "closure", NA),
    c("initial closure check", "closure", "23.2.7"),
    c("^variable [^ ]+ is not declared", "closure", NA),
    # data files
    c("header .* not found", "data", NA),
    c("not found in the data file", "data", NA),
    c("cannot open", "data", NA),
    # factorization workspace / memory limits (solve_drivers.c
    # ma48_grow_la + ma48_alloc_fail, block_solve.c/block_order.c
    # growth loops); sits above the numeric block, whose "did not
    # converge" wording would otherwise claim these
    c("MA48 workspace", "resource", NA),
    c("MA48 could not factorize", "resource", NA),
    # Jacobian preallocation ceiling (solve_drivers.c jac_mat_prealloc):
    # the system exceeds the solver's PetscInt index width, a size
    # limit rather than a workspace one -- remedies differ
    c("ceiling of the [0-9]+-bit PetscInt build", "size", NA),
    c("could not preallocate the", "size", NA),
    # HSL_MP48 (SBBD) takes INTEGER(4) counts: hsl_kernels.f90 hsl_i4
    # aborts past that range, the same size class with the same remedy
    c("32-bit HSL MP48 interface", "size", NA),
    # runtime numeric evaluation
    c("linear solve gave a value that is not finite", "numeric", "34.1"),
    c("not finite", "numeric", "34.3"),
    c("not satisfied very accurately", "numeric", "30.6.1"),
    c("zero divided by zero", "numeric", "10.11.1"),
    c("division by zero in a formula", "numeric", "10.11.1"),
    c("assertion failed", "numeric", "25.3"),
    c("has an? (updated )?value (at or )?(above|below) its declared", "numeric", "25.4.4"),
    c("fractional power of a negative number", "numeric", NA),
    c("zero pivot", "numeric", "14.1.10"),
    # statement surface (Tier A: strong comments, loop keywords,
    # constants, signed powers, product updates)
    c("strong comment", "tab", "11.1.5"),
    c("(LOOP|BREAK|CYCLE) statements are not supported", "tab", "11.18"),
    c("numeric constant .* is out of the supported range", "tab", "11.4.9"),
    c("after (expanding exponent-notation|bracketing signed powers)", "tab", NA),
    c("product Update of", "tab", "11.12.4"),
    # left-hand sides and index offsets (formula.c lhs_args_bind,
    # offset_range_check, parse_index_leadlag; recursion order)
    c("backward recursion", "tab", "16.5"),
    c("index offsets are not allowed on the left-hand side", "tab", "11.11.4"),
    c("index offset .* is not an integer constant", "tab", "11.2.4"),
    c("index offset .* runs outside set", "tab", "16.4"),
    c("the left-hand side of .* (carries [0-9]+ argument|has an empty or malformed argument)", "tab", "10.8"),
    c("is not an index of the statement's quantifiers", "tab", "10.8"),
    # names (names_validate)
    c("or a declared subset of it", "tab", "10.1.2"),
    c("is not a declared subset of", "tab", "10.1.2"),
    c("declared as both a", "tab", "11.2.1"),
    c("declared more than once", "tab", "11.2.1"),
    c("is a reserved word", "tab", "11.2.1"),
    c("collides with the change column", "tab", NA),
    c("share a name \\(p_/c_ prefixes", "tab", NA),
    # declaration qualifiers (tab_qualifiers_parse)
    c("unknown (variable|coefficient) qualifier", "tab", "10.3"),
    c("qualifier NO_SPLIT", "tab", "10.3"),
    c("LINEAR_NAME=", "tab", "10.3"),
    c("empty qualifier", "tab", "10.3"),
    c("unbalanced parentheses in .* qualifier", "tab", "10.3"),
    # bounds + Default statements
    c("duplicate (lower|upper) bound", "tab", "10.19.1"),
    c("bound defaults are not supported", "tab", "10.19"),
    c("under Equation \\(default=levels\\)", "tab", "10.19"),
    c("default=add_homotopy\\) is not supported", "tab", "10.19"),
    c("unknown (coefficient|variable|formula|equation) default", "tab", "10.19"),
    c("Default statements apply only", "tab", "10.19"),
    # PostSim sections
    c("PostSim", "tab", "12.2"),
    # sets
    c("references itself in a set expression", "tab", "10.1.1.1"),
    c("defined as equal to itself", "tab", "10.1.2.1"),
    c("set difference subtracts a larger set", "tab", NA),
    c("intertemporal set", "tab", NA),
    c("element range abbreviation", "tab", NA),
    c("more elements than", "tab", "10.1.1.1"),
    c("in the definition of", "tab", "10.1.1.1"),
    c("set .* is not declared", "tab", NA),
    c("not a declared set", "tab", "10.1.2.1"),
    c("malformed .*set declaration", "tab", NA),
    c("malformed element list", "tab", NA),
    # set product builder (tab_parse.c): joined element name past NAMESIZE
    c("set product element .* exceeds", "tab", NA),
    # formula.c formula_bind_operand: an index or quoted element used as
    # an arithmetic operand
    c("cannot be an arithmetic operand", "tab", NA),
    c("negative size in TAB file", "tab", NA),
    c("elements of set .* are not in set", "tab", NA),
    # intrinsics / formula compilation
    c("POS function is not supported", "tab", "11.5.6"),
    c("takes exactly 2 arguments", "tab", "11.5"),
    c("takes at least 2 arguments", "tab", "11.5.1"),
    c("arguments? in an intrinsic function call", "tab", "11.5"),
    c("Formula & Equation", "tab", "10.9.1"),
    c("malformed formula", "tab", NA),
    c("malformed if\\(\\)", "tab", NA),
    c("formula too long to compile", "tab", NA),
    # reference shape (formula_bind_operand index-count guard,
    # jacobian.c eq_linearity_check; 2026-09-06 ECO_TOY report)
    c("is declared with [0-9]+ ind(ex|ices) but is referenced with", "tab", "11.4.10"),
    c("is not linear in its variables", "tab", "11.4.8"),
    # formula.c conditional-quantifier binding (GTAP-W scalar conditions)
    c("quantifier condition refers to", "tab", "11.4.11"),
    c("quantifier condition .* carries", "tab", "11.4.11"),
    c("contains no linear variable", "tab", "11.4.8"),
    # reads
    c("Read without a header", "tab", "11.11.8"),
    c("from terminal is not supported", "tab", "10.6"),
    c("is not a declared variable, coefficient, or parameter", "tab", "10.6"),
    c("malformed (partial )?Read statement", "tab", "10.6"),
    # condensation / backsolve
    c("backsolv", "tab", "14.1"),
    c("must not reach the solver", "tab", NA)
  )
  map <- as.data.frame(
    do.call(rbind, rows),
    stringsAsFactors = FALSE
  )
  names(map) <- c("pattern", "class", "manual")
  map
}
