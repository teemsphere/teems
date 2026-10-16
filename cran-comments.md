## R CMD check results

0 errors | 0 warnings | 0 notes

## General testing

All tests here (except revdep) have been conducted (https://github.com/ThinkR-open/prepare-for-cran).

## Solver

The solver runs in a locally built Docker image (teems-solver 1.1.0, built from the user's own HSL license), so tests and examples that solve a model skip on CRAN. They are run before each release on Linux and Windows machines with the image installed.
