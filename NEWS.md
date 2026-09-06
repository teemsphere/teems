# teems (development version)
* `ems_model()` names the coefficient when a value is passed for one that is neither read nor assigned by a formula (the message printed the literal `nme`); an unreachable duplicate of that check was removed
* Runge-Kutta rework: stages combine in log-levels by default (`chart = "log"` in `ems_RK()`), so a percentage-change variable cannot reach -100% at any stage; the accept test is steered by percentage-change variables (`scope = "pct"`), first-same-as-last stage reuse, stage-level range-test/assertion retries, `error_norm`, `controller`, `h_init` and `guard` controls, and `"Heun"` as a second-order method; `ems_RK()` defaults to `eps_tolerance = 0.01` (measured: the accuracy of the previous driver for about a third of the factorizations)
* The embedded Runge-Kutta error estimate is returned as `error_estimate` (was `error_metric`) and read from `sol.est` (was `sol.acc`): it is an indicator of the least-settled elements, not a bound
* `model_diagnostics.txt` records the Runge-Kutta chart, norm, scope and controller, and the run's step and stage-solve counts with the rejection census
* Solver aborts at the Jacobian index ceiling (a system whose one-rank Jacobian passes 2^31 nonzeros under `matrix_method = "LU"` or `"SBBD"`) surface as a size error naming the distributed `"DBBD"` layout, condensation, or smaller dimensions as the remedies, instead of a segfault exit status
* Conditional set builders `Set X = (all,i,SRC: COEF(i,...) <op> <constant>);` (GEMPACK manual 10.1.2) are supported: the elements are evaluated from the deployed data at `ems_deploy()` and re-evaluated by the solver
* `Read (IfHeaderExists)` is supported; absent headers are not required in the loaded data
* IF conditions may compare two coefficient expressions (`IF[THETA(j,r)*ONOFF(j,r) <= 0, ...]`), and several set-membership/element IF terms may partition one Equation on a shared index
* Tablo source conventions accepted by GEMPACK are normalised: keywords glued to their bracket (`Coefficient(all,r,REG)`), `![[! !]]!` block comments, empty statements and labels, Latin-1/CRLF/BOM files
* Set products (`x`), `$POS`, and Sets built from a `full_exclude`d coefficient abort with named messages (the latter was silently dropped before)
* Coefficients return through the solver's binary coefficient dump (`sol.cof`/`sol.cbin`) instead of per-coefficient CSV files: `ems_compose()` reads them selectively and exactly (the CSVs carried six fixed decimals); `ems_deploy(write_coefficients = TRUE)` restores the CSV `Write` pairs
* `ems_solve()` checks the solver's exit status in addition to scanning its log
* `ems_model(auto_omit = TRUE)` omits unshocked, wholly exogenous variables at deploy time
* `ems_solve()` reports when a condensed deployment meets a bordered method, or is intertemporal
* `ems_probe()` returns a condensation verdict read from the measured block structure
* Deploy metadata records the condensation state and the exogenous/variable element counts

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