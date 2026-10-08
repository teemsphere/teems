# solve_in_situ errors when model directory doesn't exist

    Code
      solve_in_situ(GTAPDATA = GTAPDATA, GTAPINT = GTAPINT, GTAPSETS = GTAPSETS,
        model_file = model_files[["model_file"]], closure_file = model_files[[
          "closure_file"]], model_dir = file.path(insitu_dir, "no_dir"), shock_file = shock_file,
        solution_method = "Gragg", matrix_method = "SBBD", n_subintervals = 1,
        n_tasks = 1)
    Condition
      Error in `solve_in_situ()`:
      x The `model_dir` provided '<cache>/in_situ/diff_dir/no_dir' does not exist.

# solve_in_situ errors when missing input file

    x Required files "GTAPINT", "GTAPSETS", "GTAPDATA", and "GTAPPARM" not all provided; missing: "GTAPPARM".

# solve_in_situ errors when input file is without name

    x Input files provided to `...` must be named as they appear within the `model_file`.

# solve_in_situ errors when an input file does not exist

    x Input file not found: not_a_file.txt.

# solve_in_situ errors when no input files are given

    x No input files loaded; all files must be passed as named arguments via `...`.

# solve_in_situ errors when ignore_condense is not a single TRUE or FALSE

    x `ignore_condense` must be "TRUE" or "FALSE".

