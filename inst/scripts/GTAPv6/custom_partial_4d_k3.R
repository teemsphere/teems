# 4D variable (atall: MARG_COMMm x TRAD_COMMi x REGr x REGs), 1 dim fixed
# Uses TRAD_COMM="services" mapping so MARG_COMMm has 3 elements (atp, otp, wtp).
#
# reduce_shock picks free_idx=[MARG_COMMm, REGr, REGs] (k=3, blk=1 col)
#   [1,3,4] factor=3*3*3=27 > all size-2 subsets (max 3*3=9) -> tried first
# write_ragged: blk=REGs(3), row=REGr(3), col=MARG_COMMm(3) -> 3 slices of 3x3 matrices
# stride ordering: free_idx=[1,3,4], full_dimsizes=[3,1,3,3]
#   antidim=[1,1,3,9] -> strides=[1,3,9] -> write_order=[REGs,REGr,MARG]

.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  TRAD_COMM = "services",
  ENDW_COMM = "labor_agg",
  target_format = target_format
)

model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
TRAD_COMM <- services
MARG_COMM <- c("atp", "otp", "wtp")

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

# Fix TRAD_COMMi="crops"; free dims MARG_COMMm(3) x REGr(3) x REGs(3), n=27
# reduce_shock: free_idx=[1,3,4] factor=27 > all size-2 subsets (max 9) -> k=3
atall <- atall_full[atall_full$TRAD_COMMi == "crops", ]
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
