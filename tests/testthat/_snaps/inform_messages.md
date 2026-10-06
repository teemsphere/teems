# loaded data reports its version, reference year and format

    Code
      .inform_metadata(list(full_database_version = "GTAPv11c", reference_year = 2017,
        data_format = "GTAPv7"))
    Message
      i GTAP Data Base version: GTAPv11c
      i Reference year: 2017
      i Data format: GTAPv7

# EFVE overrides are reported

    Code
      .inform_efve(metadata = list(efve_overrides = overrides), call = NULL)
    Message
      i EFVE (ELFVAEN) keeps its aggregated value in "gas/chn": the fossil supply recalibration gives 0.0105, below 0.1.
      i A natural-resource rent that small cannot carry the SPLY target; the recalibrated nest would drive the resource price through zero under a moderate shock.
      i EFVE (ELFVAEN) is set to 1 in "peakload/chn" and "peakload/usa": every member of the aggregate carries the database placeholder 1e-06.
      i A value-added-energy nest whose members buy no energy has no substitution to describe; the placeholder would make the price of an unused factor run away.

# a finished run reports its elapsed time and accuracy

    Code
      .inform_diagnostics(elapsed_time = c(0, 0, 75.4), model_log = model_log,
      run_dir = run_dir, call = NULL)
    Message
      i Elapsed time: 1m 15s
      i 98% of variables accurate to at least 4 digits.

