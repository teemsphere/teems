# ems_calculate requires its input files

    x No input files loaded; all files must be passed as named arguments via `...`.

---

    x Required files "GTAPSETS", "GTAPDATA", and "GTAPPARM" not all provided; missing: "GTAPDATA" and "GTAPPARM".

---

    x Input file not found: not_a_file.txt.

---

    x Input files provided to `...` must be named as they appear within the `model_file`.

---

    Code
      ems_calculate(model_file, GTAPSETS = inputs$GTAPSETS, model_dir = file.path(
        write_dir, "absent"))
    Condition
      Error in `ems_calculate()`:
      x The `model_dir` provided '<cache>/calculate/absent' does not exist.

