# 4D variable (atall: MARG_COMMm x TRAD_COMMi x REGr x REGs), 1 dim fixed
# With MARG_COMMm size=1, reduce_shock picks free_idx=[REGr, REGs] (k=2, no blk)
#   [3,4] (size-2, factor=9) ties [1,3,4] (size-3, factor=9) but smaller size wins
# write_ragged: dcast row=REGs ~ col=REGr -> 3x3 matrix
# stride ordering: free_idx=[3,4], full_dimsizes=[1,1,3,3]
#   antidim=[1,1,1,3] -> strides=[1,3] -> write_order=[REGs,REGr] (col=REGr, row=REGs)

.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  TRAD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg",
  target_format = target_format
)

model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
TRAD_COMM <- c("svces", "food", "crops", "mnfcs", "livestock")
MARG_COMM <- "svces"

set.seed(42)

atall_full <- expand.grid(
  MARG_COMMm = MARG_COMM,
  TRAD_COMMi = TRAD_COMM,
  REGr = REG,
  REGs = REG,
  stringsAsFactors = FALSE
)
atall_full$Value <- 0
atall_full <- atall_full[do.call(order, atall_full), ]

# Fix TRAD_COMMi="crops"; free dims MARG_COMMm(1) x REGr(3) x REGs(3), n=9
# reduce_shock: [3,4] factor=9 ties [1,3,4] factor=9 -> smaller size first -> k=2, free_idx=[3,4]
atall <- atall_full[atall_full$TRAD_COMMi == "crops", ]
atall$Value <- runif(nrow(atall))

atall_shk <- ems_custom_shock(var = "atall", input = atall)

ems_option_set(write_sub_dir = "custom_partial_4d_1fixed")

cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = atall_shk
)

outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
atall <- merge(atall, atall_full, by = setdiff(colnames(atall), "Value"), all.y = TRUE)
atall$Value.x <- ifelse(is.na(atall$Value.x), atall$Value.y, atall$Value.x)
atall <- atall[, seq_len(ncol(atall) - 1)]
colnames(atall)[ncol(atall)] <- "Value"

check <- all.equal(atall, outputs$dat$atall, check.attributes = FALSE, tolerance = 1e-6)
