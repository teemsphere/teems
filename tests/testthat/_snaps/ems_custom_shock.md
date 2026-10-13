# ems_custom_shock errors when var is missing

    x argument `var` is missing, with no default

# ems_custom_shock errors when input is missing

    x argument `input` is missing, with no default

# ems_custom_shock errors when var is not character

    x `var` must be a character, not a number.

# ems_custom_shock errors when input is numeric

    x `input` must be a character or data.frame, not a number.

# ems_custom_shock errors when input data frame lacks Value column

    x a <data.table> object supplied as a shock must have Value as the last column.
    i Call: `ems_custom_shock(var = "aoall", input = no_val)`

# ems_custom_shock errors when both year and int set are provided

    x Either Year or intertemporal set ALLTIMEt should be provided, not both.
    i Call: `ems_custom_shock(var = "pop", input = extra_col)`

# ems_custom_shock errors when invalid year is provided

    x 1 tuple in the provided custom shock contain invalid Year specifications: 1: row 2 2016.
    i Call: `ems_custom_shock(var = "pop", input = invalid_year)`

# ems_custom_shock errors when input is missing sets

    x Shock input for pop is missing sets: ALLTIMEt.
    i Call: `ems_custom_shock(var = "pop", input = missing_set)`

# ems_custom_shock errors when invalid tuple is provided

    x Some shock tuples contain elements outside their respective sets: 1: not_an_ele 0.
    i Call: `ems_custom_shock(var = "pop", input = invalid_tup)`

# ems_custom_shock errors when some shock tuples are endogenous

    x Shock applied to non-exogenous tuples: 1: capital row 1.
    i Call: `ems_custom_shock(var = "qe", input = invalid_tup)`

# ems_custom_shock errors on duplicated tuples

    x Shock input for pop lists a tuple more than once: (row,0).
    i Each component may be shocked only once; remove the duplicated rows.

# custom and uniform shocks sharing a component abort

    x 2 shocks on pop overlap; a component is shocked more than once: (row,1).
    i A component may be shocked only once; the solver does not let a later shock override an earlier one.
    i Split the shocks so their components are disjoint, or combine them into a single `ems_custom_shock()`.

# a custom shock on a scalar variable points to ems_uniform_shock (R4-B8)

    x pfactwld is a scalar variable, so it has no set columns to shock by tuple.
    i Shock it with `ems_uniform_shock()` instead.

# a full custom shock on a partly swapped-out variable names the shock call (R4-B6)

    x pop is not fully exogenous; full shocks require full exogeneity.
    i Shock only its exogenous components, or swap the endogenous ones in with `ems_swap()`.
    i Call: `ems_custom_shock(var = "pop", input = expand.grid(REGr = c("chn", ` and ` "usa", "row"), ALLTIMEt = 0:2, Value = 1, stringsAsFactors = FALSE))`

# ems_custom_shock errors when the variable is fully endogenous

    x No exogenous components in qgdp; partial shocks require at least one.
    i Call: `ems_custom_shock(var = "qgdp", input = data.frame(REGr = "row", ` and ` ALLTIMEt = 0, Value = 1))`

# custom shock values must be finite numbers

    x 1 shock value is not a finite number (row 1 of the input).
    i Every Value must be a finite number; a text or missing entry in the input is not accepted.

---

    x 1 shock value is not a finite number (row 2 of the input).
    i Every Value must be a finite number; a text or missing entry in the input is not accepted.

