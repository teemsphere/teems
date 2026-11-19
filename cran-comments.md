## R CMD check results

0 errors | 0 warnings | 0 notes

## General testing

All tests here (except revdep) have been conducted (https://github.com/ThinkR-open/prepare-for-cran).

## This update

1.1.0 adds a step budget for the adaptive Runge-Kutta methods (an adaptive run that stops advancing now aborts with advice instead of running without end), a notice when a newer solver release exists for the user's image, and checks that turn two silent behaviours into errors: Johansen with several subintervals, and a range qualifier on a linear variable. It also fixes the shock file name, which grew with the number of shocked components and exceeded the file-name limit for large uniform shocks.

## Solver

The solver runs in a locally built Docker image (teems-solver 1.2.1, built from the user's own HSL license), so tests and examples that solve a model skip on CRAN. They are run before each release on Linux, Windows and macOS machines with the image installed.
