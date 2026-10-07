skip_on_cran()

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "postsim_test")
if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE, tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model_files <- ems_example("GTAPv7", write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

ps_section <- paste(
  "PostSim (Begin);",
  "Coefficient PSKBSUM # postsim: world capital stock #;",
  "Formula PSKBSUM = sum(r,REG, VKB(r));",
  "Assertion PSKBSUM gt 0;",
  "PostSim (End);",
  sep = "\n"
)
tab_txt <- readChar(model_file, file.info(model_file)$size)
writeChar(paste0(tab_txt, "\n", ps_section, "\n"), model_file, eos = NULL)

model <- ems_model(model_file, closure_file)

test_that("PostSim section rows are tagged in the model tibble", {
  expect_true("postsim" %in% names(model))
  # the GTAPv7 fixture carries the upstream report blocks; look at the
  # grafted section only
  flagged <- model[which(model$postsim & grepl("PSKBSUM", model$tab)), ]
  expect_identical(nrow(flagged), 3L)
  expect_setequal(flagged$type, c("Coefficient", "Formula", "Assertion"))
  expect_true(
    "PSKBSUM" %in% model$name[which(model$postsim & model$type == "Coefficient")]
  )
})

test_that("finalized TAB re-wraps PostSim executables in a trailing section", {
  tab <- .finalize_tab(model)
  lines <- strsplit(tab, "\n")[[1]]
  begin <- grep("^PostSim \\(Begin\\);", lines)
  end <- grep("^PostSim \\(End\\);", lines)
  expect_length(begin, 1L)
  expect_length(end, 1L)
  inside <- lines[(begin + 1):(end - 1)]
  expect_true(any(grepl("^Formula PSKBSUM", inside)))
  expect_true(any(grepl("^Assertion PSKBSUM", inside)))
  # the PostSim coefficient is declared inside the section (the solver
  # classifies PostSim coefficients by declaration site, 12.2.1) and,
  # being a PostSim-only name, never gets an ordinary Write pair
  expect_true(any(grepl("^Coefficient PSKBSUM", inside)))
  expect_false(any(grepl("PSKBSUM", lines[seq_len(begin - 1)])))
  tab_csv <- .finalize_tab(model, write_coefficients = TRUE)
  csv_lines <- unlist(strsplit(strsplit(tab_csv, "\n")[[1]], "\n"))
  begin <- grep("^PostSim \\(Begin\\);", csv_lines)
  end <- grep("^PostSim \\(End\\);", csv_lines)
  inside <- csv_lines[(begin + 1):(end - 1)]
  expect_true(any(grepl("^Write VKB to file", csv_lines[seq_len(begin - 1)])))
  expect_false(any(grepl("PSKBSUM", csv_lines[seq_len(begin - 1)])))
  expect_true(any(grepl("^File \\(new\\) PSKBSUM", inside)))
  expect_true(any(grepl("^Write PSKBSUM to file PSKBSUM", inside)))
})

test_that("PostSim coefficients get outdata entries in out/coefficients", {
  w <- .writeout(model = model, write_dir = write_dir)
  expect_true(any(grepl("out/coefficients/PSKBSUM\\.csv", w)))
  expect_true(any(grepl("out/coefficients/VKB\\.csv", w)))
  expect_false(any(grepl("out/postsim", w)))
})

test_that("forbidden statements in a PostSim section abort", {
  bad_file <- file.path(write_dir, "bad.tab")
  bad_txt <- sub(
    "PostSim \\(End\\);",
    "Update (all,r,REG) pop(r) = pop(r);\nPostSim (End);",
    paste0(tab_txt, "\n", ps_section, "\n")
  )
  writeChar(bad_txt, bad_file, eos = NULL)
  expect_snapshot_error(
    .process_tablo(tab_file = bad_file, quiet = TRUE, call = NULL)
  )
})

test_that("PostSim runs end-to-end through deploy, solve and compose", {
  skip_if(!nzchar(Sys.getenv("GTAP12_dat")), "GTAP data not available")
  skip_if(!.docker_image_present(paste0("teems:", .resolve_docker_tag())),
    "teems image not available"
  )
  dat <- ems_data(
    dat_input = Sys.getenv("GTAP12_dat"),
    par_input = Sys.getenv("GTAP12_par"),
    set_input = Sys.getenv("GTAP12_set"),
    REG = "big3", ACTS = "macro_sector", ENDW = "labor_agg"
  )
  cmf_path <- ems_deploy(dat, model)
  expect_false(any(grepl("PSKBSUM", readLines(cmf_path))))
  out <- ems_solve(cmf_path)
  # the GTAPv7 fixture's own report blocks return alongside the graft
  ps <- out[out$type == "postsim", ]
  expect_true(all(c("PSKBSUM", "WELFARE") %in% ps$name))
  ps <- ps[ps$name == "PSKBSUM", ]
  expect_identical(nrow(ps), 1L)
  ps_val <- as.numeric(ps$dat[[1]][["Value"]])
  vkb <- out[out$name == "VKB", ]
  expect_equal(ps_val, sum(vkb$dat[[1]][["Value"]]), tolerance = 1e-6)
})

test_that("PostSim coefficients reach out/coefficients CSVs with write_coefficients", {
  skip_if(!nzchar(Sys.getenv("GTAP12_dat")), "GTAP data not available")
  skip_if(!.docker_image_present(paste0("teems:", .resolve_docker_tag())),
    "teems image not available"
  )
  dat <- ems_data(
    dat_input = Sys.getenv("GTAP12_dat"),
    par_input = Sys.getenv("GTAP12_par"),
    set_input = Sys.getenv("GTAP12_set"),
    REG = "big3", ACTS = "macro_sector", ENDW = "labor_agg"
  )
  cmf_path <- ems_deploy(dat, model, write_coefficients = TRUE)
  ps_csv <- file.path(dirname(cmf_path), "out", "coefficients", "PSKBSUM.csv")
  expect_true(any(grepl("PSKBSUM.csv", readLines(cmf_path), fixed = TRUE)))
  out <- ems_solve(cmf_path)
  expect_true(file.exists(ps_csv))
  expect_false(dir.exists(file.path(dirname(cmf_path), "out", "postsim")))
  csv_val <- as.numeric(readLines(ps_csv)[2])
  ps <- out[out$type == "postsim" & out$name == "PSKBSUM", ]
  expect_equal(csv_val, as.numeric(ps$dat[[1]][["Value"]]), tolerance = 1e-6)
  cof <- file.path(dirname(cmf_path), "out", "variables", "bin", "sol.cof")
  file.rename(cof, paste0(cof, ".bak"))
  withr::defer(file.rename(paste0(cof, ".bak"), cof))
  csv_out <- ems_compose(cmf_path, which = c("PSKBSUM", "VKB"))
  expect_identical(csv_out$type[csv_out$name == "PSKBSUM"], "postsim")
  expect_identical(unique(csv_out$type[csv_out$name == "VKB"]), "coefficient")
})
