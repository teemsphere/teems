# ems_uniform_shock errors when var is missing

    x argument `var` is missing, with no default

# ems_uniform_shock errors when value is missing

    x argument `value` is missing, with no default

# ems_uniform_shock errors when var is not character

    x `var` must be a character, not a number.

# ems_uniform_shock errors when value is not numeric

    x `input` must be a numeric, not a string.

# ems_uniform_shock subsets are provided as named lists

    x Subset arguments in `...` must be named pairs: `SETi = "set_element"` or `SETi = c("set_element1", "set_element2")`.
    i Note that set names consist of the concatenation of the set name and variable-specific lowercase index.
    i Call: `ems_uniform_shock(var = "aoall", value = -1, "chn")`

---

    x Subset arguments in `...` must be named pairs: `SETi = "set_element"` or `SETi = c("set_element1", "set_element2")`.
    i Note that set names consist of the concatenation of the set name and variable-specific lowercase index.
    i Call: `ems_uniform_shock(var = "aoall", value = -1, China)`

# ems_uniform_shock errors when variable is not present in the model file

    x Shock variable "not_a_var" not found in the model.
    i Call: `ems_uniform_shock(var = "not_a_var", value = 1)`

# ems_uniform_shock errors when a set is specified that does not belong to a variable

    x Set REGs is not associated with "pop".
    i Set designations within teems comprise the variable-specific uppercase set name and lowercase index.
    i For "pop" these include: REGr and ALLTIMEt.
    i In intertemporal models, Year may be provided in lieu of an intertemporal set.
    i Call: `ems_uniform_shock(var = "pop", value = 1, REGs = "row")`

# ems_uniform_shock errors when both int set and year are provided

    x Either Year or intertemporal set ALLTIMEt should be provided, not both.
    i Call: `ems_uniform_shock(var = "pop", value = 1, ALLTIMEt = 0, Year = 2017)`

# ems_uniform_shock errors when full var shock is applied to not fully exogenous variable

    x qe is not fully exogenous; full shocks require full exogeneity.
    i If fully exogenous with multiple closure entries, consolidate into a single "qe" entry.
    i Call: `ems_uniform_shock(var = "qe", value = 1)`

# ems_uniform_shock errors when part var shock is applied to fully endogenous variable

    x No exogenous components in qfd; partial shocks require at least one.
    i Call: `ems_uniform_shock(var = "qfd", value = 1, REGr = "row")`

# ems_uniform_shock errors when invalid year provide

    x 2014 is not among the valid years for the provided time steps: 2017, 2018, and 2019.
    i Call: `ems_uniform_shock(var = "pop", value = 1, REGr = "row", Year = 2014)`

# ems_uniform_shock errors when invalid elements are

    x "not_an_ele" is not an element or subset belonging to set REG.
    i Valid elements: "chn", "row", and "usa".
    i Valid subsets: NA.
    i Call: `ems_uniform_shock(var = "pop", value = 1, REGr = "not_an_ele")`

# ems_uniform_shock errors when endogenous components are allocated shock

    x Shock allocated to non-exogenous tuples: 1: capital chn 1, 2: capital row 1, and 3: capital usa 1.
    i Call: `ems_uniform_shock(var = "qe", value = 1, ENDWMSe = "capital", ` and ` ALLTIMEt = 1)`

