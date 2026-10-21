## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE: Days since last update: 4. See "This update" below.

## General testing

All tests here (except revdep) have been conducted (https://github.com/ThinkR-open/prepare-for-cran).

## This update

1.0.1 follows 1.0.0 (2026-10-17) after a short interval to fix a bug on macOS (Docker Desktop): in 1.0.0, a second model run into the same folder within a second of the previous one fails with "cannot open sol.var".

## Solver

The solver runs in a locally built Docker image (teems-solver 1.1.0, built from the user's own HSL license), so tests and examples that solve a model skip on CRAN. They are run before each release on Linux, Windows and macOS machines with the image installed.
