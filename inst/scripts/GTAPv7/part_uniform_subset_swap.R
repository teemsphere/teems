# load GTAP HAR files, apply set mappings, and aggregate data
.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  target_format = target_format
)

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# define a uniform shock on a subset of qfd elements
partial <- ems_uniform_shock(
  var = "qxs",
  COMMc = "MARG",
  REGs = "chn",
  value = -1
)

# prepare a partial closure swap making qfd subset exogenous
qxs <- ems_swap(
  var = "qxs",
  COMMc = "MARG",
  REGs = "chn"
)

# prepare a partial closure swap making tfd subset endogenous
txs <- ems_swap(
  var = "txs",
  COMMc = "MARG",
  REGs = "chn"
)

# set the output subdirectory name within write_dir
ems_option_set(write_sub_dir = "part_uniform_subset_swap")

# validate inputs, write solver files, with mixed partial and full variable swaps
cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = partial,
  swap_in = qxs,
  swap_out = txs
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
exo_shk <- outputs$dat$qxs[REGs == "chn" & COMMc == "svces"]$Value == -1
endo1 <- outputs$dat$qxs[!(REGs == "chn" & COMMc == "svces")]$Value != 0
endo2 <- outputs$dat$txs[REGs == "chn" & COMMc == "svces"]$Value != 0
exo_null <- outputs$dat$txs[!(REGs == "chn" & COMMc == "svces")]$Value == 0

checks <- c(exo_shk, endo1, endo2, exo_null)
