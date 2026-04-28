# 4D variable (atall: MARGm x COMMc x REGs x REGd), 1 dim fixed
# Uses COMM="services" mapping so MARGm has 3 elements (atp, otp, wtp).
#
# reduce_shock picks free_idx=[MARGm, REGs, REGd] (k=3, blk=1 col)
#   [1,3,4] factor=3*3*3=27 > all size-2 subsets (max 3*3=9) -> tried first
# write_ragged: blk=REGd(3), row=REGs(3), col=MARGm(3) -> 3 slices of 3x3 matrices
# stride ordering: free_idx=[1,3,4], full_dimsizes=[3,1,3,3]
#   antidim=[1,1,3,9] -> strides=[1,3,9] -> write_order=[REGd,REGs,MARGm]

.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "services",
  ENDW = "labor_agg",
  target_format = target_format
)

model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
COMM <- services
MARG <- c("atp", "otp", "wtp")

set.seed(42)

atall_full <- expand.grid(
  MARGm = MARG,
  COMMc = COMM,
  REGs = REG,
  REGd = REG,
  stringsAsFactors = FALSE
)
atall_full$Value <- 0
atall_full <- atall_full[do.call(order, atall_full), ]

# Fix COMMc="crops"; free dims MARGm(3) x REGs(3) x REGd(3), n=27
# reduce_shock: free_idx=[1,3,4] factor=27 > all size-2 subsets (max 9) -> k=3
atall <- atall_full[atall_full$COMMc == "crops", ]
atall$Value <- runif(nrow(atall))

atall_shk <- ems_custom_shock(var = "atall", input = atall)

ems_option_set(write_sub_dir = "custom_partial_4d_k3")

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
