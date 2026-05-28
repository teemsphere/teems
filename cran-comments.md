## R CMD check results

0 errors | 0 warnings | 0 notes

## General testing

All tests here (except revdep) have been conducted (https://github.com/ThinkR-open/prepare-for-cran).

## Resubmission

### Regarding use of \dontrun{}
* A comment is present prior to any example that cannot be run detailing why and how to get started
* More examples now run and more descriptions are available
* Function examples that write, write to tempdir()

### Regarding default paths and writing to the users home space
* All default paths from writing functions have been removed
* `ems_deploy()` now writes to tempdir()
* Using `terra::terraOptions()` as a template, the default write path for `ems_deploy()` can be changed by the user
* `ems_example()` now has no default value for the `path` arg