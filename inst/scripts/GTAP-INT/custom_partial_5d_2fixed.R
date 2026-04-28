# 5D variable (atall: MARG_COMMm x TRAD_COMMi x REGr x REGs x ALLTIMEt), 2 dims fixed
#
# reduce_shock picks free_idx=[REGs, ALLTIMEt] (k=2, no blk)
#   [4,5] (size-2, factor=12) is tried before [1,4,5] (size-3, same factor=12)
# write_ragged: dcast row=ALLTIMEt ~ col=REGs -> 4x3 matrix
# stride ordering: free_idx=[4,5], full_dimsizes=[1,1,1,3,4]
#   antidim=[1,1,1,1,3] -> strides=[1,3] -> write_order=[ALT,REGs] (col=REGs, row=ALT)
# old broken code would produce 3x4 (REGs rows x ALT cols) -- detectable mismatch

time_steps <- c(0, 1, 2, 3)

.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  PROD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg",
  time_steps = time_steps,
  target_format = target_format
)

model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
TRAD_COMM <- c("svces", "food", "crops", "mnfcs", "livestock")
MARG_COMM <- "svces"
ALLTIME <- seq(0, length(time_steps) - 1)

set.seed(42)

atall_full <- expand.grid(
  MARG_COMMm = MARG_COMM,
  TRAD_COMMi = TRAD_COMM,
  REGr = REG,
  REGs = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)
atall_full$Value <- 0
atall_full <- atall_full[do.call(order, atall_full), ]

# Fix TRAD_COMMi="crops" AND REGr="chn"; free dims REGs(3) x ALLTIMEt(4), n=12
# reduce_shock: 3-element subsets need >=36 rows; [4,5] factor=12 is first size-2 match -> k=2
atall <- atall_full[atall_full$TRAD_COMMi == "crops" & atall_full$REGr == "chn", ]
atall$Value <- runif(nrow(atall))

atall_shk <- ems_custom_shock(var = "atall", input = atall)

ems_option_set(write_sub_dir = "custom_partial_5d_2fixed")

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
