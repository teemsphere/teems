# ems_scenario_shock errors when var is missing

    x argument `var` is missing, with no default

# ems_scenario_shock errors when input is missing

    x argument `input` is missing, with no default

# ems_scenario_shock errors when var is not character

    x `var` must be a character, not a number.

# ems_scenario_shock errors when input is numeric

    x `input` must be a character or data.frame, not a number.

# ems_scenario_shock errors when input data frame lacks Value column

    x a <data.table> object supplied as a shock must have Value as the last column.
    i Call: `ems_scenario_shock(var = "pop", input = no_val)`

# ems_scenario_shock errors when no year df provided

    x a <data.table> object supplied as a scenario shock must have a Year column consistent with selected time steps.
    i Call: `ems_scenario_shock(var = "pop", input = no_year)`

# ems_scenario_shock errors when used with a static model

    x Scenario shocks can only be employed with a temporally dynamic model.
    i Call: `ems_scenario_shock(var = "pop", input = shock)`

# ems_scenario_shock errors when not all preaggregation tuples provided

    x 1 tuple missing from the scenario shock: 1: afg 2023.
    i Scenario shocks must cover all pre-aggregation tuples for associated sets.
    i Call: `ems_scenario_shock(var = "pop", input = pop)`

