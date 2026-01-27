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

Computable general equilibrium models provide insights into an incredibly wide range of topics from trade to the environment. Putting together and running model runs is a difficult and time-consuming process. The `teems` package addresses these obstacles while providing a reproducible and fast platform for CGE model runs.
# teems R package
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- [![Version](https://img.shields.io/badge/version-0.9-green.svg)](https://github.com/username/repo/releases) -->

Beta version 0.0.0.98 - use with caution, currently under development!
Beta manual available [here](https://teemsphere.github.io/) - also under development!

Note that the [teems-solver](https://github.com/teemsphere/teems-solver) must be built to gain full functionality from this package.

## Installation
Install R if it is not yet on your system and an IDE like [RStudio](https://posit.co/download/rstudio-desktop/) for interactive code execution
```bash
sudo apt install r-base
```

Install the R package {remotes} and any dependencies flagged
```R
install.packages("remotes")
```

Install the latest release "v0.0.0.98" using {remotes}
```R
remotes::install_github("teemsphere/teems-R@v0.0.0.98")
```

The installation may fail, citing "there is no package called ...". Install those packages and try again.

Sample scripts are located at the [teems-scripts](https://github.com/teemsphere/teems-scripts) repository. 

## License
This project is licensed under the GPLv3.0 License - see the [LICENSE](LICENSE) file for details.

## Contact
- Project Maintainer: [Matthew Cantele](mailto:matthew.cantele@protonmail.com)
- Project Homepage: [https://github.com/teemsphere/teems-R](https://github.com/teemsphere/teems-R)
- Package Manual: [https://teemsphere.github.io/](https://teemsphere.github.io/)
<!-- - Bug Reports: [https://github.com/username/repository/issues](https://github.com/username/repository/issues) -->