# teems 0.0.4
## Major change
* Solver and package now working on Windows 11
## Bug fixes
* Windows path handling corrected throughout (`winslash = "/"`, `normalizePath`)
* Docker detection and error messaging now platform-aware (Linux vs. Windows/Mac)
* Format conversion no-ops when target format matches loaded data format
* `Value` column presence check made robust to column ordering
* Mixed-case input data now handled correctly in set mappings

## Other changes
* `CGDS` sector renamed to `zcgds` throughout for consistency with solver output (note that this will change in the next version)
* cpp11 binary parser ported to Windows: `long int` replaced with `int64_t`/`R_xlen_t`
* Solver log and run summary (elapsed time, accuracy) now written to `model_diagnostics.txt`
* ANSI codes stripped from diagnostic file output; run timestamp added
* `run_dir` parameter added to `.inform_diagnostics`

# teems 0.0.3
## Bug fixes
* Innumerable bug fixes due to the comprehensive test suite 
* NEWS will be more informative once we're on CRAN

# teems 0.0.2
## Bug fixes
* The export tag from ems_solve was dropped in the previous version

## Other changes
* Objects created with shock functions are now copied so no changes via reference are passed back

# teems 0.0.1
## Bug fixes
* Passes devtools::check()

## Other changes
* All shock functions get their own function rather than S3 dispatch
* Tests are about 70% there

# teems 0.0.0.99
## Bug fixes
* Complete revamp for CRAN submission
* Far too many changes/fixes to document

# teems 0.0.0.98

## Bug fixes
* Messy closure files now correctly parsed, omission via ! added
* Tablo statement check logic updated to fix file, outFile confusion
* stderr now properly captured in outfile and console output
* Directory creation with ems_deploy() `write_dir` now safely nested within additional folder
* Disparate header/coefficient situation with intertemporal headers fixed
* Variable/coefficient set parsing error on messy tabs fixed

## Other changes
* Can now accommodate coefficients, variables, and sets of any combination of lower and upper case
* File (new) are automatically dropped from Tablo declarations
* Write declarations automatically dropped from Tablo declarations
* Binary switches to select set elements now flagged at processing
* Coefficient and variable set order in declaration can now differ from actual set order
* ems_get_option() `full_exclude` now also used to exclude on Tablo files, removing e.g., DVER
* Tablo statement syntax now case insensitive
* Gaps in set specification (all,r,  REG) now handled
* NEWS added and to be maintain by release
* Intertemporal header checks now conducted on `n_timestep_header` and `timestep_header` (see ?ems_get_option)

# teems 0.0.0.97 (initial beta release)