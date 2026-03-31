# 3D variable (afeall: ENDWe x ACTSa x REGr), 1 dim fixed
# reduce_shock picks free_idx=[ENDWe, REGr] (k=2, no blk)
# write_ragged: dcast row=REGr ~ col=ENDWe -> 3x4 matrix
# stride ordering: free_idx=[1,3], full_dimsizes=[4,1,3]
#   antidim=[1,1,4] -> strides=[1,4] -> write_order=[REGr,ENDWe] (col=ENDWe, row=REGr)

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
ENDW <- c("labor", "capital", "natlres", "land")
ACTS <- c("svces", "food", "crops", "mnfcs", "livestock")

set.seed(42)

afeall_full <- expand.grid(
  ENDWe = ENDW,
  ACTSa = ACTS,
  REGr = REG,
  stringsAsFactors = FALSE
)
afeall_full$Value <- 0
afeall_full <- afeall_full[do.call(order, afeall_full), ]

# Fix ACTSa="crops"; free dims are ENDWe(4) x REGr(3), n=12
# reduce_shock: free_idx=[1,3] factor=12 is the only size-2 subset -> k=2
afeall <- afeall_full[afeall_full$ACTSa == "crops", ]
afeall$Value <- runif(nrow(afeall))

afeall_shk <- ems_custom_shock(var = "afeall", input = afeall)

ems_option_set(write_sub_dir = "custom_partial_3d_1fixed")

cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = afeall_shk
)

outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
afeall <- merge(afeall, afeall_full, by = setdiff(colnames(afeall), "Value"), all.y = TRUE)
afeall$Value.x <- ifelse(is.na(afeall$Value.x), afeall$Value.y, afeall$Value.x)
afeall <- afeall[, seq_len(ncol(afeall) - 1)]
colnames(afeall)[ncol(afeall)] <- "Value"

check <- all.equal(afeall, outputs$dat$afeall, check.attributes = FALSE, tolerance = 1e-6)
