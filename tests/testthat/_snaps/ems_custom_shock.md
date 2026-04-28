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

    ! Could not evaluate cli `{}` expression: `time_set`.
    Caused by error in `eval(expr, envir = envir)`:
    ! object 'time_set' not found

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

