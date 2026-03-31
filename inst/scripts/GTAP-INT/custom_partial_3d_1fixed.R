# 3D variable (aoall: PROD_COMMj x REGr x ALLTIMEt), 1 dim fixed
# reduce_shock picks free_idx=[PROD_COMMj, ALLTIMEt] (k=2, no blk)
# write_ragged: dcast row=ALLTIMEt ~ col=PROD_COMMj -> 4x6 matrix
# stride ordering: antidim([PROD,ALT]) = [1,6] -> write_order=[ALT,PROD] (col=PROD, row=ALT)
# old broken code would produce 6x4 (PROD rows x ALT cols) -- detectable mismatch

time_steps <- c(0, 1, 2, 3)

.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  TRAD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg",
  time_steps = time_steps,
  target_format = target_format
)

model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
PROD_COMM <- c("svces", "food", "crops", "mnfcs", "livestock", "zcgds")
ALLTIME <- seq(0, length(time_steps) - 1)

set.seed(42)

aoall_full <- expand.grid(
  PROD_COMMj = PROD_COMM,
  REGr = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)
aoall_full$Value <- 0
aoall_full <- aoall_full[do.call(order, aoall_full), ]

# Fix REGr="chn"; free dims are PROD_COMMj(6) x ALLTIMEt(4), n=24
# reduce_shock: free_idx=[1,3] factor=24 is largest -> one het_grp -> k=2
aoall <- aoall_full[aoall_full$REGr == "chn", ]
aoall$Value <- runif(nrow(aoall))

aoall_shk <- ems_custom_shock(var = "aoall", input = aoall)

ems_option_set(write_sub_dir = "custom_partial_3d_1fixed")

cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = aoall_shk
)

outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
aoall <- merge(aoall, aoall_full, by = setdiff(colnames(aoall), "Value"), all.y = TRUE)
aoall$Value.x <- ifelse(is.na(aoall$Value.x), aoall$Value.y, aoall$Value.x)
aoall <- aoall[, seq_len(ncol(aoall) - 1)]
colnames(aoall)[ncol(aoall)] <- "Value"

check <- all.equal(aoall, outputs$dat$aoall[, !"Year"], check.attributes = FALSE, tolerance = 1e-6)
