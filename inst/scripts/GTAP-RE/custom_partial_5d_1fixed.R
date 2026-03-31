# 5D variable (atall: MARGm x COMMc x REGs x REGd x ALLTIMEt), 1 dim fixed
#
# reduce_shock picks free_idx=[REGs, REGd, ALLTIMEt] (k=3, blk=1 col)
#   [3,4,5] (size-3, factor=36) is tried before [1,3,4,5] (size-4, same factor=36)
#   because equal-factor subsets preserve original list order (smaller size first)
# write_ragged: blk=ALLTIMEt(4), row=REGd(3), col=REGs(3) -> 4 slices of 3x3
# stride ordering: free_idx=[3,4,5], full_dimsizes=[1,1,3,3,4]
#   antidim=[1,1,1,3,9] -> strides=[1,3,9] -> write_order=[ALT,REGd,REGs]
#
# Note: MARGm has only 1 element with this mapping, so [3,4,5] (size-3)
#   is always selected over [1,3,4,5] (size-4). The k=4 branch (blk=2 cols)
#   requires MARG size > 1 and is not reachable with this model configuration.

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
COMM <- c("svces", "food", "crops", "mnfcs", "livestock")
MARG <- "svces"
ALLTIME <- seq(0, length(time_steps) - 1)

set.seed(42)

atall_full <- expand.grid(
  MARGm = MARG,
  COMMc = COMM,
  REGs = REG,
  REGd = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)
atall_full$Value <- 0
atall_full <- atall_full[do.call(order, atall_full), ]

# Fix COMMc="crops"; free dims REGs(3) x REGd(3) x ALLTIMEt(4), n=36
# reduce_shock: free_idx=[3,4,5] factor=36 > all size-2 factors (max 20) -> k=3
atall <- atall_full[atall_full$COMMc == "crops", ]
atall$Value <- runif(nrow(atall))

atall_shk <- ems_custom_shock(var = "atall", input = atall)

ems_option_set(write_sub_dir = "custom_partial_5d_1fixed")

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

check <- all.equal(atall, outputs$dat$atall[, !"Year"], check.attributes = FALSE, tolerance = 1e-6)
