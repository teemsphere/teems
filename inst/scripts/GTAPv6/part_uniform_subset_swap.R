# load GTAP HAR files, apply set mappings, and aggregate data
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  PROD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg"
)

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# define a uniform shock on a subset of qfd elements
partial <- ems_uniform_shock(
  var = "qfd",
  REGs = c("usa", "chn"),
  PROD_COMMj = "TRAD_COMM",
  value = -1
)

# prepare a partial closure swap making qfd subset exogenous
qfd <- ems_swap(
  var = "qfd",
  REGs = c("usa", "chn"),
  PROD_COMMj = "TRAD_COMM"
)

# prepare a partial closure swap making tfd subset endogenous
tfd <- ems_swap(
  var = "tfd",
  REGr = c("usa", "chn"),
  PROD_COMMj = "TRAD_COMM"
)

# set the output subdirectory name within write_dir
ems_option_set(write_sub_dir = "part_uniform_subset_swap")

# validate inputs, write solver files, with mixed partial and full variable swaps
cmf_path <- ems_deploy(
  write_dir = write_dir,
  dat = dat,
  model = model,
  shock = partial,
  swap_in = qfd,
  swap_out = tfd
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
exo_shk <- outputs$dat$qfd[(REGs %in% c("usa", "chn") & PROD_COMMj != "cgds")]$Value == -1
endo1 <- outputs$dat$qfd[(!REGs %in% c("usa", "chn") & PROD_COMMj == "cgds")]$Value != 0
endo2 <- outputs$dat$tfd[(REGr %in% c("usa", "chn") & PROD_COMMj != "cgds")]$Value != 0
exo_null <- outputs$dat$tfd[REGr %in% c("usa", "chn") & PROD_COMMj == "cgds"]$Value == 0

checks <- c(exo_shk, endo1, endo2, exo_null)
