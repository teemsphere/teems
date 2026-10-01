# teems 1.0.0

## Breaking changes
* `ems_solve()`/`solve_in_situ()`: `solution_method` defaults to `"Gragg"` (was `"Johansen"`); `"mod_midpoint"` is renamed `"Gragg"` (it was always effectively Gragg); `steps` defaults by method
* `laA`, `laD`, `laDi` and `append_args` are removed as named arguments; the MA48 workspace sizes and expert solver flags are validated `...` arguments
* `ems_model(var_omit = )` is removed; in-TAB `Omit` statements are ignored with a message
* GTAP-INT is removed; GTAP-RE is replaced by version 2 on the GTAPv7.1 core
* Element names are case-insensitive on input and lowercase in all outputs
* teems checks the solver image version and aborts on a major-version mismatch or an image below 1.1.0 (`ems_option_set(version_check = )`)

## New functions
* `ems_probe()`: structural probe of a deployed model; recommends a matrix method and resources and gives a condensation verdict
* `ems_RK()`: Runge-Kutta solution methods (RK2, Heun, RK4, BoSha32, DoPri54) with adaptive step control
* `ems_complementarity()`: run controls for models with `Complementarity` statements
* `ems_calculate()`: runs a TAB's formulas without a simulation (e.g., data file)

## New models and data handling
* Vetted models: GTAPv7.1 (updated from GTAPv7.0), GTAP-AEZ, GTAP-E, GTAP-EP (GTAP-Power), ORANI-G
* `ems_data()` loads a single-HAR database (`par_input`/`set_input` optional)
* `ems_data(par_weights = )`: `"share"` (default) or `"value"` (the FlexAgg rule) weights for parameter aggregation, overridable per parameter
* Closure swaps were applied ins-before-outs, rejecting valid sequences; two swaps in on one tuple wrote duplicate closure entries

## Solving
* `solution_method` gains `"Euler"` and the Runge-Kutta family; Euler and Gragg accept a single step count (no extrapolation), and Gragg accepts all-odd counts
* Condensation: in-TAB `Substitute`/`Backsolve` statements and `ems_model(backsolve = , ignore_condense = )`
* `ems_solve()` gains `n_threads`, `precision = "double"`, `verbosity` and `complementarity`
* Run-mode switches (assertions, range tests) are set with `ems_option_set()`
* Coefficients are read from the solver's binary dump at full precision (previously CSVs with six fixed decimals); `ems_deploy(write_coefficients = TRUE)` restores the CSVs
* Solver failures are reported by class (resource, size, model specification, interface) instead of a generic exit status

## TABLO coverage
* Set expressions, conditional set builders, `$POS`, `Mapping`, levels variables and equations, `Complementarity`, `PostSim`, IF terms in formulas and equations, `Read (IfHeaderExists)`
* Malformed TABs abort by name at model load instead of inside the solver

## Bug fixes affecting results
* `ems_uniform_shock()` on a one-index variable restricted to an element or subset shocked the wrong elements (element 0 / the first n elements of the full set) (solver 1.1.0)
* DPSM (all GTAP models) was summed over aggregated regions, so an aggregated region held the number of subsumed original GTAP regions instead of 1
* GTAPv7: the scalar `pxwwld` was dropped from `E_c1_cr`, `E_cnttotr` and `E_cntpinv` because the solver did not recognise a variable followed by a closing bracket (solver 1.1.0)
* DBBD solves left residuals that multi-step runs accumulated; every DBBD solve now takes one refinement step (solver 1.1.0)
* Data rounding used fixed decimals, writing small values (below 5e-7) as 0; values now keep `ndigits` significant digits
* Whole-valued Real data at or above 2^31 was written as empty fields
* Sets read from a header, and explicit set lists, were written in alphabetical rather than file/declaration order, misaligning data indexed over unions and changing `$POS` arithmetic (no effect on the shipped GTAP models)
* Custom and scenario shocks with duplicated or overlapping rows were accepted and could shock elements never specified
* Redeploying within the same minute appended to the existing shock file
* A user shock file with an argument-less `uniform` shock shocked only the variable's first element (solver 1.1.0)
* A uniform shock on a scalar variable declared without sets was written as `Shock phi(NA)`
* GTAP-RE: the `PVALFWDTIME` discount did not scale with time-step length

## Platform
* On Linux the solver container runs as the calling user (previously failed for users other than uid 1000)
* Deploy paths containing spaces work
* A deploy directory accepts one solve at a time
* The Docker image tag is chosen from the host CPU

# teems 0.1.1
* `ems_example()` example typo fixed
* verbatim tests for all exported function examples added

# teems 0.1.0
* `write_dir` removed from `ems_deploy()`, default write directory now tempdir() and can be overridden via `tempdir` arg in `ems_option_set()`
* Explicit `path` first arg for `ems_example()`, with no default
* `ems_example()` now accepts outputs from `GTAP_convert()`
* `GTAP_convert()` inputs now har-specific
* Expanded and more descriptive examples

# teems 0.0.5
* Officially submitted to CRAN