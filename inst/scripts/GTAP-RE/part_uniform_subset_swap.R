# load GTAP HAR files, apply set mappings, and aggregate data
time_steps <- c(0, 1, 2)
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = time_steps
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
  REGs = c("chn", "usa"),
  ALLTIMEt = "FWDTIME",
  value = -1
)

# prepare a partial closure swap making qfd subset exogenous
qxs <- ems_swap(
  var = "qxs",
  COMMc = "MARG",
  REGs = c("chn", "usa"),
  ALLTIMEt = "FWDTIME"
)

# prepare a partial closure swap making tfd subset endogenous
txs <- ems_swap(
  var = "txs",
  COMMc = "MARG",
  REGs = c("chn", "usa"),
  ALLTIMEt = "FWDTIME"
)


# validate inputs, write solver files, with mixed partial and full variable swaps
cmf_path <- ems_deploy(
  .data = dat,
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
exo_shk <- outputs$dat$qxs[REGs %in% c("chn", "usa") & COMMc == "svces" & ALLTIMEt != length(time_steps) - 1]$Value == -1
endo1 <- outputs$dat$qxs[!(REGs %in% c("chn", "usa") & COMMc == "svces" & ALLTIMEt != length(time_steps) - 1)]$Value != 0
endo2 <- outputs$dat$txs[REGs %in% c("chn", "usa") & COMMc == "svces" & ALLTIMEt != length(time_steps) - 1]$Value != 0
exo_null <- outputs$dat$txs[!(REGs %in% c("chn", "usa") & COMMc == "svces" & ALLTIMEt != length(time_steps) - 1)]$Value == 0
qxs_len_check <- (length(exo_shk) + length(endo1)) == nrow(outputs$dat$qxs)
txs_len_check <- (length(endo2) + length(exo_null)) == nrow(outputs$dat$txs)
checks <- c(exo_shk, endo1, endo2, exo_null, qxs_len_check, txs_len_check)
