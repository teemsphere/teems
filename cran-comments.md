## R CMD check results

0 errors | 0 warnings | 0 notes

## General testing

All tests here (except revdep) have been conducted (https://github.com/ThinkR-open/prepare-for-cran).

## This update

1.0.1 follows 1.0.0 (2026-10-17) after a short interval: it fixes a failure on macOS (Docker Desktop), where a second model run into the same folder within about a second of the previous one could not open its solution files, and quiets the default console output of `ems_solve()` while keeping the full solver log on file. No API change.

## Solver

The solver runs in a locally built Docker image (teems-solver 1.1.0, built from the user's own HSL license), so tests and examples that solve a model skip on CRAN. They are run before each release on Linux, Windows and macOS machines with the image installed.
