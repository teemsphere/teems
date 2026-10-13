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
    i To shock only some aggregated tuples, supply every pre-aggregation tuple behind each of them, in every year, and set `partial = TRUE`.
    i Call: `ems_scenario_shock(var = "pop", input = pop)`

# a scenario shock with a zero base-year value aborts

    x 9 scenario shock values for pop are not finite: chn,0, chn,1, chn,2, row,0, row,1, row,2, usa,0, usa,1, and usa,2.
    i A scenario shock is the percentage change from the base-year value, which is zero or missing for these tuples.

# ems_scenario_shock errors when partial is not a single TRUE or FALSE

    x `partial` must be "TRUE" or "FALSE".

---

    x `partial` must be a logical, not a string.

# a partial scenario shock covering whole aggregated tuples deploys

    x 483 tuples missing from the scenario shock: 1: afg 2023, 2: afg 2024, 3: afg 2025, 4: ago 2023, 5: ago 2024, --- , 479: zmb 2024, 480: zmb 2025, 481: zwe 2023, 482: zwe 2024, and 483: zwe 2025.
    i Scenario shocks must cover all pre-aggregation tuples for associated sets.
    i To shock only some aggregated tuples, supply every pre-aggregation tuple behind each of them, in every year, and set `partial = TRUE`.
    i Call: `ems_scenario_shock(var = "pop", input = traj)`

# a partial scenario shock covering part of an aggregated tuple aborts

    x 1 aggregated tuple of pop is only partly covered by the scenario shock: row.
    i A partial scenario shock needs every pre-aggregation tuple, in every year, behind each aggregated tuple it shocks; 474 are missing: are,2023, are,2024, are,2025, arg,2023, arg,2024, arg,2025, arm,2023, arm,2024, arm,2025, and aus,2023.
    i Call: `ems_scenario_shock(var = "pop", input = traj, partial = TRUE)`

# a scenario shock tuple outside the pre-aggregation sets aborts

    x 3 scenario shock tuples for pop match no pre-aggregation element or time step: zzz,2023, zzz,2024, and zzz,2025.
    i Call: `ems_scenario_shock(var = "pop", input = traj)`

