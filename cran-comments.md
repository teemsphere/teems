## R CMD check results

0 errors | 0 warnings | 0 notes

## General testing

All tests here (except revdep) have been conducted (https://github.com/ThinkR-open/prepare-for-cran).

## Deferred functions

`ems_calculate()`, `ems_homogeneity()` and `ems_complementarity()` (and the `complementarity` argument of the solve functions) are developed on the `dev` branch and withheld from this release; the solver features behind them ship in teems-solver 1.1.0 and stay reachable through its command file.
