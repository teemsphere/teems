# teems <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R >= 4.3.0](https://img.shields.io/badge/R-%E2%89%A5%204.3.0-276DC3?logo=r)](https://cran.r-project.org/)
<!-- badges: end -->

**Trade and Environment Equilibrium Modeling System** --- an open-source R
package for reproducible Computable General Equilibrium (CGE) model runs.

TEEMS provides a complete pipeline from raw GTAP data through model solution:
data loading, aggregation, model parsing, shock specification, and
Docker-based solving --- all from R.

> **Status:** Beta (v0.0.0.99) --- under active development. Use with caution.

## Overview

| Function | Purpose |
|:---------|:--------|
| `ems_aux()` | Load auxiliary HAR, data frame, CSV data
| `ems_data()` | Load HAR and CSV input data, apply set mappings and time steps, aggregate and convert data |
| `ems_model()` | Parse model inputs (TABLO files) and load closure |
| `ems_shock()` | Define shocks (uniform, custom, scenario) |
| `ems_swap()` | Load closure swaps |
| `ems_deploy()` | Validate inputs and write solver files |
| `ems_solve()` | Execute the Docker-based solver |
| `ems_compose()` | Parse solver outputs into structured R objects |
| `ems_option_set()`/`ems_option_get()` | Access advanced CGE model customization options

## Installation

### Prerequisites

- **R** (>= 4.3.0)
- **Docker** --- required for `ems_solve()` (see [teems-solver](https://github.com/teemsphere/teems-solver))
- **Solver build** --- required for `ems_solve()` (see [teems-solver](https://github.com/teemsphere/teems-solver))

### Install from GitHub

```r
# install.packages("remotes")
remotes::install_github("teemsphere/teems-R@v0.0.0.98")
```

If the installation fails citing a missing package, install that package first
and retry.

## Quick start

```r
library(teems)

# 1. Load and aggregate data
data <- ems_data(
  dat_input = "path/to/gsdfdat.har",
  par_input = "path/to/gsdfpar.har",
  set_input = "path/to/gsdfset.har",
  timesteps = c(2017, 2018, 2019, 2020),
  REG  = "AR5",
  COMM = "macro_sector",
  ACTS = "macro_sector",
  ENDW = "labor_agg"
)

# 2. Configure model and closure
model <- ems_model(model_input = "path/to/model",
                   closure_file = "path/to/closure")

# 3. Define shocks
shock <- ems_shock(var = "pop",
                   type = "uniform",
                   REGr = "lam",
                   value = 1)

# 4. Validate and write solver inputs
cmf_path <- ems_deploy(.data = data,
                       model = model,
                       shock = shock)

# 5. Solve
results <- ems_solve(cmf_path = cmf_path,
                     n_tasks = 2,
                     n_subintervals = 6,
                     matrix_method = "SBBD",
                     solution_method = "mod_midpoint")
```

See the [teems-scripts](https://github.com/teemsphere/teems-scripts) repository
for complete worked examples.

## Data requirements

TEEMS works with [GTAP](https://www.gtap.agecon.purdue.edu/) FlexAgg data.

## Documentation

Full documentation is available at
**[teemsphere.github.io](https://teemsphere.github.io/)**.

## Related repositories

| Repository | Description |
|:-----------|:------------|
| [teems-models](https://github.com/teemsphere/teems-models) | Vetted models and their standard closures |
| [teems-solver](https://github.com/teemsphere/teems-solver) | C/Fortran optimization solver (Docker) |
| [teems-scripts](https://github.com/teemsphere/teems-scripts) | Example scripts for GTAP model versions |
| [teems-mappings](https://github.com/teemsphere/teems-mappings) | Regional and sectoral aggregation mappings |
| [teems-manual](https://github.com/teemsphere/teems-manual) | Full package documentation |

## License

GPL-3.0 --- see [LICENSE.md](LICENSE.md) for details.

## Contact

**Matthew Cantele** --- [matthew.cantele@protonmail.com](mailto:matthew.cantele@protonmail.com)
