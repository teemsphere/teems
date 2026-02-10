---
output: github_document
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

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

> **Status:** Beta (v0.0.0.985) --- under active development. Use with caution.

## Overview

| Function | Purpose |
|:---------|:--------|
| `ems_data()` | Load HAR files, apply set mappings, aggregate data |
| `ems_model()` | Parse TABLO `.tab` file and load closure configuration |
| `ems_shock()` | Define shocks (uniform, custom, scenario) |
| `ems_swap()` | Configure closure swaps |
| `ems_deploy()` | Validate inputs and write solver files |
| `ems_solve()` | Execute the Docker-based solver |
| `ems_compose()` | Parse solver outputs into structured R objects |

## Installation

### Prerequisites

- **R** (>= 4.3.0)
- **Docker** --- required for `ems_solve()` (see [teems-solver](https://github.com/teemsphere/teems-solver))

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
  REG  = "AR5",
  COMM = "macro_sector",
  ACTS = "macro_sector",
  ENDW = "labor_agg"
)

# 2. Configure model and closure
model <- ems_model(model_input = "GTAPv7.0")

# 3. Define shocks
shock <- ems_shock(var = "pop", type = "uniform", value = 1)

# 4. Validate and write solver inputs
cmf_path <- ems_deploy(data = data, model = model, shock = shock)

# 5. Solve
results <- ems_solve(cmf_path = cmf_path)
```

See the [teems-scripts](https://github.com/teemsphere/teems-scripts) repository
for complete worked examples.

## Data requirements

TEEMS works with [GTAP](https://www.gtap.agecon.purdue.edu/) FlexAgg data.
GTAP 9 data is open access; versions 10 and 11 require a GTAP membership.

## Documentation

Full documentation is available at
**[teemsphere.github.io](https://teemsphere.github.io/)**.

## Related repositories

| Repository | Description |
|:-----------|:------------|
| [teems-solver](https://github.com/teemsphere/teems-solver) | C/Fortran optimization solver (Docker) |
| [teems-scripts](https://github.com/teemsphere/teems-scripts) | Example scripts for GTAP model versions |
| [teems-mappings](https://github.com/teemsphere/teems-mappings) | Regional and sectoral aggregation mappings |
| [teems-manual](https://github.com/teemsphere/teems-manual) | Quarto-based documentation source |

## License

GPL-3.0 --- see [LICENSE.md](LICENSE.md) for details.

## Contact

**Matthew Cantele** --- [matthew.cantele@protonmail.com](mailto:matthew.cantele@protonmail.com)
