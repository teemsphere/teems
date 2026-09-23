# NOTE (2026-09-24): this script used to swap the full qfd vector for tfd
# with a small qfd shock. GTAP 12 gives forestry (frs) a land input (about
# 180 bn USD across big3; none in GTAP 11), so under the macro_sector mapping
# the mnfcs aggregate carries a sluggish land factor and the full qfd/tfd
# swap becomes near-singular on that database: tfd moved by tens of percent
# for a 0.1 % qfd shock and the Gragg ladder could not converge (the same
# swap is well behaved on GTAP 10 and 11). The full swap is now qgdp for
# afreg with a small qgdp shock on one region.

# load GTAP HAR files, apply set mappings, and aggregate data
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg"
)


# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# define a uniform shock on a single region's qgdp
partial <- ems_uniform_shock(
  var = "qgdp",
  REGr = "usa",
  value = 0.1
)

# validate inputs, write solver files, with full variable swaps passed as strings
cmf_path <- ems_deploy(
  .data = dat,
  model = model,
  shock = partial,
  swap_in = "qgdp",
  swap_out = "afreg"
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Gragg"
)

# checks
# multi-step solutions carry rounding, so values are compared within a tolerance
exo_shk <- abs(outputs$dat$qgdp[REGr == "usa"]$Value - 0.1) < 1e-6
exo_null <- abs(outputs$dat$qgdp[REGr != "usa"]$Value) < 1e-6
endo <- outputs$dat$afreg$Value != 0
qgdp_len_check <- (length(exo_shk) + length(exo_null)) == nrow(outputs$dat$qgdp)
afreg_len_check <- length(endo) == nrow(outputs$dat$afreg)
checks <- c(exo_shk, exo_null, endo, qgdp_len_check, afreg_len_check)
