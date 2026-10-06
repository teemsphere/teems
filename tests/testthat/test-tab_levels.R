skip_on_cran()

# C0-R (teems-solver docs/mapping_complementarity_design.md section 5):
# levels statements through the R pipeline. The solver linearizes
# Equation (levels) by change differentiation and expands
# Formula&Equation (tab_levels_transform, teems-solver 126698d); R
# splits Formula&Equation into its two 10.9.1 halves at the statement
# stage, parses Equation (levels) qualifiers ahead of the name, and
# mirrors the solver's c_-leading levels-name fatal (p_-leading names
# are carried by the C1a gen_lv pair rename). Solver ground truth:
# teems-solver/.audit/levels-test-kit.

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "tab_levels")
if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE, tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model_files <- ems_example("GTAPv7", write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]
base_txt <- readChar(model_file, file.info(model_file)$size)

mutate_tab <- function(text, name = "mut.tab") {
  path <- file.path(write_dir, name)
  writeChar(paste0(base_txt, "\n", text, "\n"), path, eos = NULL)
  path
}

expect_preflight_error <- function(text) {
  expect_snapshot_error(
    .process_tablo(tab_file = mutate_tab(text), quiet = TRUE, call = NULL)
  )
}

levels_block <- paste(
  "Variable (levels) LX # levels percent operand #;",
  "Formula (initial) LX = 2;",
  "Variable (levels) LY;",
  "Formula (initial) LY = 3;",
  "Variable (levels) LZ;",
  "Formula (initial) LZ = 6;",
  "Equation (levels) E_LZ # levels product # LZ = LX * LY;",
  "Variable (change,levels) CA;",
  "Formula (initial) CA = 2;",
  "Variable (change,levels) CB;",
  "Formula (initial) CB = 3;",
  "Variable (change,levels) CC;",
  "Formula (initial) CC = 6;",
  "Equation (levels) E_CC CC = CA * CB;",
  "Variable (change,levels) (all,r,REG) LV(r) # summed operand #;",
  "Formula (initial) (all,r,REG) LV(r) = 1;",
  "Variable (change,levels) LW2 # sum via Formula&Equation #;",
  "Formula&Equation E_LW2 LW2 = sum(r,REG, LV(r));",
  sep = "\n"
)

# --- model stage -------------------------------------------------------

test_that("levels statements parse", {
  model <- .process_tablo(
    tab_file = mutate_tab(levels_block),
    quiet = TRUE,
    call = NULL
  )

  lz <- model[model$type == "Equation" & model$name %in% "E_LZ", ]
  expect_identical(nrow(lz), 1L)
  expect_identical(lz$qualifier_list, "(levels)")
  expect_identical(lz$label, "levels product")

  cc <- model[model$type == "Equation" & model$name %in% "E_CC", ]
  expect_identical(cc$qualifier_list, "(levels)")

  lx <- model[model$type == "Variable" & model$name %in% "LX", ]
  expect_identical(lx$qualifier_list, "(levels)")
  ca <- model[model$type == "Variable" & model$name %in% "CA", ]
  expect_identical(ca$qualifier_list, "(change,levels)")
})

test_that("Formula&Equation expands into its two 10.9.1 halves", {
  model <- .process_tablo(
    tab_file = mutate_tab(levels_block),
    quiet = TRUE,
    call = NULL
  )

  eq <- model[model$type == "Equation" & model$name %in% "E_LW2", ]
  expect_identical(nrow(eq), 1L)
  expect_identical(eq$qualifier_list, "(levels)")

  fm <- model[model$type == "Formula" &
    !is.na(model$comp1) & grepl("^LW2", model$comp1), ]
  expect_identical(nrow(fm), 1L)
  expect_identical(fm$qualifier_list, "(initial)")

  # both halves reach the deployed TAB (the solver re-linearizes the
  # Equation (levels) half)
  tab <- .finalize_tab(model)
  expect_match(tab, "Formula (initial) LW2 = sum(r,REG, LV(r))",
    fixed = TRUE
  )
  expect_match(tab, "Equation (levels) E_LW2 LW2 = sum(r,REG, LV(r))",
    fixed = TRUE
  )
})

test_that("malformed Formula & Equation aborts", {
  expect_preflight_error(
    "Formula & Equation E_BAD (all,r,REG) qgdp(r);"
  )
})

test_that("p_-leading levels variable name parses (C1a gen_lv rename)", {
  model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "Variable (levels) p_ok # carried by the solver's pair rename #;",
      "Formula (initial) p_ok = 1;",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  )
  expect_true("p_ok" %in% model$name[model$type == "Variable"])
})

test_that("c_-leading levels variable name aborts", {
  expect_preflight_error(paste(
    "Variable (levels) c_bad # value refs fold into p_ columns #;",
    "Formula (initial) c_bad = 1;",
    sep = "\n"
  ))
})

test_that("ADD_HOMOTOPY qualifiers and defaults declare their homotopy variables (manual 26.7.5)", {
  model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "Variable (levels,change) HA1;",
      "Formula (initial) HA1 = 1;",
      "Variable (levels,change) HA2;",
      "Formula (initial) HA2 = 1;",
      "Variable (levels,change) HA3;",
      "Formula (initial) HA3 = 1;",
      "Variable (levels,change) HA4;",
      "Formula (initial) HA4 = 1;",
      "Equation (default=add_homotopy=homo1);",
      "Equation (levels) E_HA1 HA1 = 2;",
      "Equation (levels, add_homotopy=homo2) E_HA2 HA2 = 3;",
      "Equation (default=not_add_homotopy);",
      "Equation (levels, add_homotopy) E_HA3 HA3 = 4;",
      "Equation (levels) E_HA4 HA4 = 1;",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  )
  eq <- model[model$type == "Equation" & model$name %in% c("E_HA1", "E_HA2", "E_HA3", "E_HA4"), ]
  expect_identical(gsub(" ", "", eq$qualifier_list[match(c("E_HA1", "E_HA2", "E_HA3", "E_HA4"), eq$name)], fixed = TRUE),
    c("(levels,add_homotopy=homo1)", "(levels,add_homotopy=homo2)", "(levels,add_homotopy)", "(levels)"))
  hv <- model[model$type == "Variable" & tolower(model$name) %in% c("homo1", "homo2", "homotopy"), ]
  expect_setequal(tolower(hv$name), c("homo1", "homo2", "homotopy"))
  expect_true(all(hv$qualifier_list == "(levels,change)"))
  tab <- .finalize_tab(model)
  expect_match(tab, "Formula (initial) homo1 = -1", fixed = TRUE)
  expect_match(tab, "Formula (initial) HOMOTOPY = -1", fixed = TRUE)
})

test_that("a homotopy variable the model declares is not declared again", {
  model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "Variable (levels,change) HOMOTOPY # declared by the model #;",
      "Formula (initial) HOMOTOPY = -1;",
      "Variable (levels,change) HB1;",
      "Formula (initial) HB1 = 1;",
      "Equation (levels, add_homotopy) E_HB1 HB1 = 2;",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  )
  expect_identical(sum(model$type == "Variable" & tolower(model$name) == "homotopy"), 1L)
})

test_that("compositions, offsets on mapped indices and LHS mappings parse (manual 11.9.6-11.9.8)", {
  model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "Set GRP (g1, g2);",
      "Set HH (h1, h2);",
      "Mapping R2G from REG to GRP;",
      "Read (by_elements) R2G from file GTAPDATA header \"R2G\";",
      "Mapping G2H from GRP to HH;",
      "Read (by_elements) G2H from file GTAPDATA header \"G2H\";",
      "Coefficient (all,h,HH) CHH(h);",
      "Formula (all,h,HH) CHH(h) = $pos(h);",
      "Coefficient (all,r,REG) CRR(r);",
      "Formula (all,r,REG) CRR(r) = CHH(G2H(R2G(r)));",
      "Coefficient (all,h,HH) CGL(h);",
      "Formula (all,h,HH) CGL(h) = 0;",
      "Formula (all,g,GRP) CGL(G2H(g)) = 5;",
      "Variable (all,h,HH) xhh(h);",
      "Variable (all,r,REG) yrr(r);",
      "Equation E_yrr (all,r,REG) yrr(r) = xhh(G2H(R2G(r)));",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  )
  expect_true(all(c("CRR", "CGL") %in% model$name[model$type == "Coefficient"]))
  expect_true("E_yrr" %in% model$name[model$type == "Equation"])
})

# --- e2e solve legs (need a teems image with the C0 levels solver,
# --- teems-solver 126698d+; run with ems_option_set(docker_tag =
# --- "dev") against a current rebuild) --------------------------------

solver_has_levels <- function() {
  img <- paste0("teems:", .resolve_docker_tag())
  if (!.docker_image_present(img)) {
    return(FALSE)
  }
  out <- suppressWarnings(system2(
    "docker",
    c(
      "run", "--rm", img, "/bin/bash", "-c",
      shQuote("grep -c 'levels equation' /opt/teems-solver/solver/teems-solver")
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
  length(out) > 0L && !is.na(suppressWarnings(as.integer(out[1]))) &&
    as.integer(out[1]) > 0L
}

skip_if_no_levels_e2e <- function() {
  skip_if(
    !solver_has_levels(),
    "teems image absent or predates the levels solver"
  )
}

skip_if(!nzchar(dat_input), "GTAP data not available")

conv <- GTAP_convert(dat_input, par_input, set_input)

lv_data <- function() {
  suppressMessages(ems_data(
    dat_input = conv$dat,
    par_input = conv$par,
    set_input = conv$set,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
}

test_that("levels equations solve to pinned values (e2e)", {
  nest_temp("levels_e2e", write_dir)
  skip_if_no_levels_e2e()
  # solver-kit values-leg shape: percent product LZ = LX*LY with p_lx
  # shocked 10 -> p_lz = 10 exactly (change diff, constant cofactor);
  # change product CC = CA*CB with c_ca shocked 0.5 -> c_cc = 1.5;
  # F&E sum LW2 over big3 REG with c_lv shocked 1 -> c_lw2 = 3
  d <- lv_data()
  model <- ems_model(mutate_tab(levels_block, name = "levels.tab"), closure_file)
  cmf_path <- ems_deploy(
    d,
    model,
    shock = list(
      ems_uniform_shock(var = "LX", value = 10),
      ems_uniform_shock(var = "CA", value = 0.5),
      ems_uniform_shock(var = "LV", value = 1)
    ),
    swap_in = c("LX", "LY", "CA", "CB", "LV")
  )
  out <- suppressMessages(ems_solve(cmf_path))
  expect_s3_class(out, "data.frame")
  pin <- function(nm) {
    as.numeric(out[tolower(out$name) == tolower(nm), ]$dat[[1]][["Value"]])
  }
  expect_equal(pin("LZ"), 10, tolerance = 1e-6)
  expect_equal(pin("CC"), 1.5, tolerance = 1e-6)
  expect_equal(pin("LW2"), 3, tolerance = 1e-6)
})

test_that("linear_name and linear_var levels variables solve by either name (e2e)", {
  nest_temp("levels_linear_e2e", write_dir)
  skip_if_no_levels_e2e()
  # LM = LN*XL with LN's linear variable named xpc (LINEAR_NAME) and
  # XL's linear variable the declared xvl (LINEAR_VAR); xpc shocked 10
  # by its linear name and XL 5 by its levels name -> p_lm = 15.5
  # exactly, and the percent-change equation yq = 2*xpc compounds to
  # yq = 1.1^2 - 1 = 21 (GEMPACK manual 9.2.2, 24.13)
  d <- lv_data()
  block <- paste(
    "Variable (levels, linear_name=xpc) (all,r,REG) LN(r);",
    "Formula (initial) (all,r,REG) LN(r) = 2;",
    "Variable (all,r,REG) xvl(r);",
    "Variable (levels, linear_var=xvl) (all,r,REG) XL(r);",
    "Formula (initial) (all,r,REG) XL(r) = 4;",
    "Variable (levels) (all,r,REG) LM(r);",
    "Formula (initial) (all,r,REG) LM(r) = 8;",
    "Equation (levels) E_LM (all,r,REG) LM(r) = LN(r) * XL(r);",
    "Variable (all,r,REG) yq(r);",
    "Equation E_yq (all,r,REG) yq(r) = 2 * xpc(r);",
    sep = "\n"
  )
  model <- ems_model(mutate_tab(block, name = "linear.tab"), closure_file)
  cmf_path <- ems_deploy(
    d,
    model,
    shock = list(
      ems_uniform_shock(var = "xpc", value = 10),
      ems_uniform_shock(var = "XL", value = 5)
    ),
    swap_in = c("LN", "xvl")
  )
  out <- suppressMessages(ems_solve(cmf_path))
  pin <- function(nm) {
    as.numeric(out[tolower(out$name) == tolower(nm), ]$dat[[1]][["Value"]])
  }
  expect_equal(pin("xpc"), rep(10, 3), tolerance = 1e-6)
  expect_equal(pin("xvl"), rep(5, 3), tolerance = 1e-6)
  expect_equal(pin("LM"), rep(15.5, 3), tolerance = 1e-4)
  expect_equal(pin("yq"), rep(21, 3), tolerance = 1e-4)
})

test_that("a sum condition in a levels equation reaches the solver as written", {
  block <- paste(
    "Variable (change,levels) (all,r,REG) LS(r) # summed operand #;",
    "Formula (initial) (all,r,REG) LS(r) = 1;",
    "Variable (change,levels) LT # conditional total #;",
    "Formula (initial) LT = 2;",
    "Equation (levels) E_LT LT = sum{r,REG: r <> \"usa\", LS(r)};",
    sep = "\n"
  )
  model <- .process_tablo(
    tab_file = mutate_tab(block),
    quiet = TRUE,
    call = NULL
  )
  tab <- .finalize_tab(model)
  expect_match(tab, "Equation (levels) E_LT LT = sum{r,REG: r <> \"usa\", LS(r)}", fixed = TRUE)
})

solver_has_homotopy <- function() {
  img <- paste0("teems:", .resolve_docker_tag())
  if (!.docker_image_present(img)) {
    return(FALSE)
  }
  out <- suppressWarnings(system2(
    "docker",
    c(
      "run", "--rm", img, "/bin/bash", "-c",
      shQuote("grep -c 'ADD_HOMOTOPY' /opt/teems-solver/solver/teems-solver")
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
  length(out) > 0L && !is.na(suppressWarnings(as.integer(out[1]))) &&
    as.integer(out[1]) > 0L
}

test_that("ADD_HOMOTOPY takes the data onto a levels equation (manual 26.7.1, e2e)", {
  nest_temp("levels_homotopy_e2e", write_dir)
  skip_if(!solver_has_homotopy(), "teems image predates ADD_HOMOTOPY")
  # V2 = V1 + K holds at the start (Formula & Equation); V1^2 + V2^2 = 5
  # does not, until HOMOTOPY moves from -1 to 0: the roots reached from
  # V1 = K + 2 are 1, (-4 + sqrt(24))/4 and -1 for K = 1, 2, 3
  d <- lv_data()
  block <- paste(
    "Variable (levels,change) (all,r,REG) HV1(r);",
    "Variable (levels,change) (all,r,REG) HV2(r);",
    "Coefficient (parameter) (all,r,REG) HK(r);",
    "Formula (initial) (all,r,REG) HK(r) = $pos(r);",
    "Formula (initial) (all,r,REG) HV1(r) = $pos(r) + 2;",
    "Formula & Equation E_HV2 (all,r,REG) HV2(r) = HV1(r) + HK(r);",
    "Equation (levels, add_homotopy) E_HV1 (all,r,REG) HV1(r)^2 + HV2(r)^2 = 5;",
    sep = "\n"
  )
  model <- ems_model(mutate_tab(block, name = "homotopy.tab"), closure_file)
  cmf_path <- ems_deploy(
    d,
    model,
    shock = ems_uniform_shock(var = "HOMOTOPY", value = 1),
    swap_in = "HOMOTOPY"
  )
  out <- suppressMessages(ems_solve(cmf_path, solution_method = "Gragg", steps = c(20L, 40L, 80L)))
  hv1 <- out$dat[[which(out$name == "HV1")]]
  expect_equal(hv1$Value, c(-2, (-4 + sqrt(24)) / 4 - 4, -6), tolerance = 1e-3)
  expect_equal(hv1$PostLevel, c(1, (-4 + sqrt(24)) / 4, -1), tolerance = 1e-3)
  homo <- out$dat[[which(out$name == "HOMOTOPY")]]
  expect_equal(homo$PostLevel, 0, tolerance = 1e-6)
})
