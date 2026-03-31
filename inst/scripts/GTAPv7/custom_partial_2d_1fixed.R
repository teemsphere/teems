# 2D variable (aoall: ACTSa x REGr), 1 dim fixed
# reduce_shock picks free_idx=[ACTSa] (k=1)
# write_ragged: single-column branch

.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  COMM = "macro_sector",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  target_format = target_format
)

model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
ACTS <- c("svces", "food", "crops", "mnfcs", "livestock")

set.seed(42)

aoall_full <- expand.grid(
  ACTSa = ACTS,
  REGr = REG,
  stringsAsFactors = FALSE
)
aoall_full$Value <- 0
aoall_full <- aoall_full[do.call(order, aoall_full), ]

# Fix REGr="chn"; free dim is ACTSa(5), n=5
# reduce_shock: free_idx=[ACTSa], one het_grp -> k=1
aoall <- aoall_full[aoall_full$REGr == "chn", ]
aoall$Value <- runif(nrow(aoall))

aoall_shk <- ems_custom_shock(var = "aoall", input = aoall)

ems_option_set(write_sub_dir = "custom_partial_2d_1fixed")

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

check <- all.equal(aoall, outputs$dat$aoall, check.attributes = FALSE, tolerance = 1e-6)
