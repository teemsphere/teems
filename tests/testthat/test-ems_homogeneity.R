skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "homogeneity")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE,
               tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

static_files <- ems_example("GTAPv7", write_dir)
static_data <- ems_data(dat_input, par_input, set_input,
                        REG = "big3", ACTS = "macro_sector", ENDW = "labor_agg")
static_model <- quiet_pivot(ems_model(static_files[["model_file"]], static_files[["closure_file"]]))

test_that("GTAPv7 is nominally and really homogeneous", {
  nest_temp("homogeneity_v7", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  for (type in c("nominal", "real")) {
    h <- ems_homogeneity(cmf_path, type = type, simulate = TRUE)
    expect_identical(h$type, type)
    expect_gt(h$typed, 150L)
    tested <- h$equations[h$equations$tested, ]
    expect_gt(nrow(tested), 100L)
    expect_lt(max(tested$max_err), 1e-5, label = paste(type, "check"))
    expect_lt(max(h$variables$max_err), 1e-5, label = paste(type, "simulation"))
    expect_gt(sum(h$variables$zero_flow), 0L)
    expect_true(all(c("equation", "element", "tested", "err", "zero_flow") %in% names(h$elements)))
  }
  expect_false(file.exists(file.path(dirname(cmf_path), "out", "variables", "bin", "sol.jac")))
})

test_that("the GTAP-RE saving-price equations are flagged", {
  nest_temp("homogeneity_re", write_dir)
  re_files <- ems_example("GTAP-RE", file.path(write_dir, "homogeneity_re"))
  re_data <- ems_data(dat_input, par_input, set_input,
                      REG = "big3", ACTS = "macro_sector", ENDW = "labor_agg",
                      time_steps = c(0, 1, 2))
  re_model <- ems_model(re_files[["model_file"]], re_files[["closure_file"]])
  h <- ems_homogeneity(ems_deploy(re_data, re_model), type = "nominal")
  failing <- h$equations$equation[which(h$equations$tested & h$equations$max_err > 1e-5)]
  expect_setequal(failing, c("e_psave", "e_walras_dem"))
  pca <- h$equations[h$equations$equation == "e_pca", ]
  expect_gt(pca$zero_flow, 0L)
  expect_lt(pca$zero_flow, pca$rows)
})

test_that("a deployment without VPQ types cannot be checked", {
  nest_temp("homogeneity_untyped", write_dir)
  text <- readLines(static_files[["model_file"]])
  text <- text[!grepl("VPQType", text)]
  untyped_file <- file.path(write_dir, "homogeneity_untyped", "untyped.tab")
  writeLines(text, untyped_file)
  cmf_path <- ems_deploy(static_data, quiet_pivot(ems_model(untyped_file, static_files[["closure_file"]])))
  expect_snapshot_error(ems_homogeneity(cmf_path))
  meta <- readRDS(file.path(dirname(cmf_path), "metadata.rds"))
  meta$vpqtype <- NULL
  saveRDS(meta, file.path(dirname(cmf_path), "metadata.rds"))
  expect_snapshot_error(ems_homogeneity(cmf_path))
  expect_snapshot_error(ems_homogeneity(cmf_path, type = "both"))
  expect_snapshot_error(ems_homogeneity(cmf_path, simulate = NA))
})

test_that("an element is a zero flow when the shifted defaults move its coefficients", {
  a <- list(nrow = 3L, row = c(0L, 0L, 1L, 2L), col = c(0L, 1L, 1L, 2L), value = c(1, -1, 2, 0.5))
  b <- a
  b$value[3] <- 2.002
  expect_identical(.homogeneity_moved_rows(a, b), c(FALSE, TRUE, FALSE))
  b <- a
  b$row <- c(b$row, 2L)
  b$col <- c(b$col, 0L)
  b$value <- c(b$value, 1e-3)
  expect_identical(.homogeneity_moved_rows(a, b), c(FALSE, FALSE, TRUE))
})
