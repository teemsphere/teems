# 4D variable (afeall: ENDWe x ACTSa x REGr x ALLTIMEt), 1 dim fixed
# reduce_shock picks free_idx=[ENDWe, REGr, ALLTIMEt] (k=3, blk=1 col)
# write_ragged: blk=ALLTIMEt(4), row=REGr(3), col=ENDWe(4) -> 4 slices of 3x4
# stride ordering: antidim([ENDW,REG,ALT]) = [1,4,12] -> write_order=[ALT,REG,ENDW]
# old broken code would order [ENDW,REG,ALT] -> wrong dim assignment

time_steps <- c(0, 1, 2, 3)

.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  COMM = "macro_sector",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = time_steps,
  target_format = target_format
)

model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
ENDW <- c("labor", "capital", "natlres", "land")
ACTS <- c("svces", "food", "crops", "mnfcs", "livestock")
ALLTIME <- seq(0, length(time_steps) - 1)

set.seed(42)

afeall_full <- expand.grid(
  ENDWe = ENDW,
  ACTSa = ACTS,
  REGr = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)
afeall_full$Value <- 0
afeall_full <- afeall_full[do.call(order, afeall_full), ]

# Fix ACTSa="crops"; free dims are ENDWe(4) x REGr(3) x ALLTIMEt(4), n=48
# reduce_shock: free_idx=[1,3,4] factor=48 is the only 3-element subset not including ACTS
#   that matches n_free=48 -> one het_grp -> k=3
afeall <- afeall_full[afeall_full$ACTSa == "crops", ]
afeall$Value <- runif(nrow(afeall))

afeall_shk <- ems_custom_shock(var = "afeall", input = afeall)

ems_option_set(write_sub_dir = "custom_partial_4d_1fixed")

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

check <- all.equal(afeall, outputs$dat$afeall[, !"Year"], check.attributes = FALSE, tolerance = 1e-6)
