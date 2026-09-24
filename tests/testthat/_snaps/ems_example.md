# ems_example errors when model is missing

    x argument `model` is missing, with no default

# ems_example errors when path is missing

    x argument `path` is missing, with no default

# ems_example errors when path does not exist

    Code
      ems_example("GTAPv7", file.path(write_dir, "not_a_dir"))
    Condition
      Error in `ems_example()`:
      x '<cache>/example/not_a_dir' does not exist or is not writable.

# ems_example errors when type is scripts and an input is missing

    x `dat_input` (with `par_input` and `set_input` for a GTAP database) must be provided when `type` is "scripts".

