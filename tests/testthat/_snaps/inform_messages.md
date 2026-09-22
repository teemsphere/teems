# loaded data reports its version, reference year and format

    Code
      .inform_metadata(list(full_database_version = "GTAPv11c", reference_year = 2017,
        data_format = "GTAPv7"))
    Message
      i GTAP Data Base version: GTAPv11c
      i Reference year: 2017
      i Data format: GTAPv7

# a finished run reports its elapsed time and accuracy

    Code
      .inform_diagnostics(elapsed_time = c(0, 0, 75.4), model_log = model_log,
      run_dir = run_dir, call = NULL)
    Message
      i Elapsed time: 1m 15s
      i 98% of variables accurate to at least 4 digits.

