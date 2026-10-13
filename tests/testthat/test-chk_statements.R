skip_on_cran()

# Front-end robustness on real-world TAB spellings (6.4 corpus
# re-survey findings B1-B5): every accepted GEMPACK spelling must
# normalise to the same parsed model as the canonical spelling, and
# malformed text must abort with a named message, never a parser crash.

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "statements_test")
if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE, tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model_files <- ems_example("GTAPv7", write_dir)
model_file <- model_files[["model_file"]]
base_txt <- readChar(model_file, file.info(model_file)$size)

write_tab <- function(text, name = "mut.tab", ...) {
  path <- file.path(write_dir, name)
  writeChar(text, path, eos = NULL, ...)
  path
}

process <- function(path) {
  .process_tablo(tab_file = path, quiet = TRUE, call = NULL)
}

base_model <- process(model_file)
attr(base_model, "tab_file") <- NULL

expect_same_model <- function(path) {
  model <- process(path)
  attr(model, "tab_file") <- NULL
  expect_identical(model, base_model)
}

test_that("keywords glued to their first parenthesis parse (B1)", {
  txt <- gsub("Coefficient (", "Coefficient(", base_txt, fixed = TRUE)
  txt <- gsub("Formula (", "Formula(", txt, fixed = TRUE)
  txt <- gsub("Variable (", "Variable(", txt, fixed = TRUE)
  txt <- gsub("Equation (", "Equation(", txt, fixed = TRUE)
  txt <- gsub("Update (", "Update(", txt, fixed = TRUE)
  expect_true(grepl("Coefficient(all", txt, fixed = TRUE))
  expect_same_model(write_tab(txt))
})

test_that("![[! !]]! block comments with nested ! are stripped (B2)", {
  txt <- paste0(
    "![[! block comment ! with a nested bang ! and\n",
    "Coefficient (all,r,REG) BOGUS(r);\n",
    "across lines !]]!\n",
    base_txt,
    "\n![[! trailing !]]!\n"
  )
  expect_same_model(write_tab(txt))
})

test_that("nested strong comments strip to the outermost pair (A5)", {
  txt <- paste0(
    "![[! outer\n",
    "Coefficient (all,r,REG) BOGUS1(r);\n",
    "![[! inner\n",
    "Coefficient (all,r,REG) BOGUS2(r);\n",
    "!]]!\n",
    "Coefficient (all,r,REG) BOGUS3(r);\n",
    "!]]!\n",
    base_txt
  )
  expect_same_model(write_tab(txt))
})

test_that("strong comment helper counts depth and keeps stray closes", {
  expect_identical(.strip_strong_comments("a![[!b![[!c!]]!d!]]!e", call = NULL), "ae")
  expect_identical(.strip_strong_comments("a![[!b!]]!c![[!d!]]!e", call = NULL), "ace")
  expect_identical(.strip_strong_comments("a !x!!]]! b", call = NULL), "a !x!!]]! b")
  expect_identical(.strip_strong_comments("no markers", call = NULL), "no markers")
})

test_that("PHILGEM nested block is fully commented out (A5)", {
  philgem <- file.path(
    "/home/mpc/src/teems/teems_tabs/potential_models/tabs",
    "PHILGEM_tpec0109.tab"
  )
  skip_if_not(file.exists(philgem))
  txt <- .tab_to_utf8(readChar(philgem, file.info(philgem)$size, useBytes = TRUE))
  out <- .strip_strong_comments(txt, call = NULL)
  expect_false(grepl("![[!", out, fixed = TRUE))
  expect_false(grepl("!]]!", out, fixed = TRUE))
  expect_false(grepl("QRATIO", out, fixed = TRUE))
  expect_false(grepl("CAPSTOK", out, fixed = TRUE))
  expect_true(grepl("End of PHILGEM", out, fixed = TRUE))
})

test_that("unclosed strong comment aborts (A5)", {
  txt <- paste0(base_txt, "\n![[! open\n![[! nested !]]!\n")
  expect_snapshot_error(process(write_tab(txt)))
})

test_that("empty statements are no-ops (B3)", {
  txt <- sub(";", ";;", base_txt, fixed = TRUE)
  txt <- paste0(txt, "\n;\n  ;\n")
  expect_same_model(write_tab(txt))
})

test_that("Latin-1 and CRLF files parse (B4)", {
  txt <- gsub("\n", "\r\n", base_txt, fixed = TRUE)
  path <- file.path(write_dir, "latin1.tab")
  con <- file(path, "wb")
  writeBin(
    c(
      charToRaw("! caf"), as.raw(c(0xE9, 0x20, 0xE0)),
      charToRaw(" la GEMPACK !\r\n"), charToRaw(txt)
    ),
    con
  )
  close(con)
  expect_same_model(path)
})

test_that("a UTF-8 BOM is dropped (B4)", {
  path <- file.path(write_dir, "bom.tab")
  con <- file(path, "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeChar(base_txt, con, eos = NULL)
  close(con)
  expect_same_model(path)
})

test_that("label text does not reach the qualifier scans (B5)", {
  txt <- paste0(
    base_txt,
    "\nCoefficient (all,r,REG) LBLX(r) # (default for zerodivide default) #;\n",
    "Formula (initial) (all,r,REG) LBLX(r) = 1;\n"
  )
  model <- process(write_tab(txt))
  expect_true("LBLX" %in% model$name)
})

test_that("stray label text outside a statement aborts", {
  txt <- sub(
    "Coefficient (all,r,REG)",
    "# stray label # Coefficient (all,r,REG)",
    base_txt,
    fixed = TRUE
  )
  expect_snapshot_error(process(write_tab(txt)))
})

test_that("declared names are canonicalised to their declared spelling", {
  st <- c(
    "Set Com # c # (Wool, Com, Grain)",
    "Set MIND = (all,i,Ind: X(i) > 0)",
    "Set IND # i # read elements from file InFile header \"IND\"",
    "File INFILE # data #",
    "Coefficient (all,c,COM)(all,i,ind) Make(c,i) # MAKE #",
    "Read MAKE from file Infile header \"Make\"",
    "Variable (all,c,com) xCom(c)",
    "Equation E_x (all,c,COM) xcom(c) = sum{i,IND, MAKE(c,i)*XCOM(c)} + y(\"com\")"
  )
  out <- .canonical_names(st)
  expect_identical(out[1], st[1])
  expect_identical(out[2], "Set MIND = (all,i,IND: X(i) > 0)")
  expect_identical(out[3], "Set IND # i # read elements from file INFILE header \"IND\"")
  expect_identical(out[5], "Coefficient (all,c,Com)(all,i,IND) Make(c,i) # MAKE #")
  expect_identical(out[6], "Read Make from file INFILE header \"Make\"")
  expect_identical(out[8], "Equation E_x (all,c,Com) xCom(c) = sum{i,IND, Make(c,i)*xCom(c)} + y(\"com\")")

  clash <- .canonical_names(c("Coefficient (all,r,REG) POP(r)", "Variable (all,r,REG) pop(r)", "Equation E_p (all,r,REG) pop(r) = POP(r)"))
  expect_identical(clash[3], "Equation E_p (all,r,REG) pop(r) = POP(r)")
})

test_that("qualifier groups are detected by position, not by substring (R1-B3, R1-B4, R1-B5)", {
  txt <- paste0(
    base_txt,
    "\nVariable (percent_change) (all,r,REG) zpc(r);",
    "\nCoefficient INITIALGDP;",
    "\nFormula INITIALGDP = 2;",
    "\nVariable LEVELSHIFT;",
    "\nEquation E_ls LEVELSHIFT = 2*pfactwld;",
    "\nCoefficient (non_parameter) (all,r,REG) NPW(r);",
    "\nRead NPW from file GTAPDATA header \"NPW\";\n"
  )
  model <- process(write_tab(txt))
  rows <- stats::setNames(model$qualifier_list, model$name)
  expect_identical(rows[["zpc"]], "(percent_change)")
  expect_true(is.na(rows[["INITIALGDP"]]))
  expect_true(is.na(rows[["LEVELSHIFT"]]))
  expect_identical(rows[["NPW"]], "(non_parameter)")
  expect_false(.qual_is_parameter(rows[["NPW"]]))
  expect_identical(.qual_is_parameter(c("(parameter)", "(real, parameter)", NA)), c(TRUE, TRUE, FALSE))
})

test_that("formula and equation bodies survive qualifier sniffing, labels and glued names (R1b-B1, R1b-B4, R1b-B5)", {
  txt <- paste0(
    base_txt,
    "\nCoefficient INITIALW;",
    "\nFormula INITIALW = 2;",
    "\nCoefficient (all,c,COMM) LEVELSUMX(c);",
    "\nFormula # copy # (all,c,COMM) LEVELSUMX(c) = 1;",
    "\nCoefficient (all,c,COMM) BINIT(c);",
    "\nFormula # initial value of B # (all,c,COMM: INITIALW ge 0) BINIT(c) = 1;",
    "\nCoefficient (all,c,COMM) CNEST(c);",
    "\nFormula (all,c,COMM: LEVELSUMX(MCOM(c)) > 0) CNEST(c) = 1;",
    "\nMapping MCOM from COMM to COMM;",
    "\nFormula (all,c,COMM) MCOM(c) = c;",
    "\nVariable (all,c,COMM) xg(c);",
    "\nEquation E_xg(all,c,COMM) xg(c) = 2*pfactwld;\n"
  )
  model <- process(write_tab(txt))
  fml <- model[model$type == "Formula" & grepl("INITIALW|LEVELSUMX|BINIT|CNEST", model$tab), ]
  expect_identical(fml$comp1, c("INITIALW", "LEVELSUMX(c)", "BINIT(c)", "CNEST(c)"))
  expect_identical(fml$comp2, c("2", "1", "1", "1"))
  expect_identical(fml$label, c(NA, "copy", "initial value of B", NA))
  expect_true(all(is.na(fml$qualifier_list)))
  expect_identical(model$name[model$type == "Equation" & grepl("xg", model$tab)], "E_xg")
})

test_that("a paren-led equation side and a comparison inside IF split at the top-level equals (R1b-B2)", {
  txt <- paste0(
    base_txt,
    "\nCoefficient (all,c,COMM) CW(c);",
    "\nFormula (all,c,COMM) CW(c) = $POS(c);",
    "\nVariable (all,c,COMM) xb3(c);",
    "\nVariable (all,c,COMM) zq(c);",
    "\nVariable (all,c,COMM) xq(c);",
    "\nEquation E_xb3 (all,c,COMM) (CW(c)+1)*xb3(c) = 2*pfactwld;",
    "\nEquation E_xq (all,c,COMM) xq(c) = 3*pfactwld;",
    "\nEquation E_zq (all,c,COMM) IF(CW(c) >= 2, CW(c))*zq(c) + zq(c) = xq(c);",
    "\nBacksolve xq using E_xq;\n"
  )
  model <- process(write_tab(txt))
  eqs <- model[model$type == "Equation" & model$name %in% c("E_xb3", "E_zq"), ]
  expect_identical(eqs$comp1, c("(CW(c)+1)*xb3(c)", "IF[CW(c)>=2, CW(c)]*zq(c) + zq(c)"))
  expect_identical(eqs$comp2, c("2*pfactwld", "3*pfactwld"))
})

test_that("a misspelt keyword after a declaration aborts; a first statement without a keyword aborts by name (R1-B6)", {
  expect_snapshot_error(.check_statements("Coefficient A; Coeficient B;", call = NULL))
  expect_snapshot_error(.check_statements("Foo X; Coefficient A;", call = NULL))
  out <- .check_statements(
    "Coefficient (all,r,REG) A(r); (all,r,REG) B(r) # lab #; C; Read A from file X header \"A\"; B from file X header \"B\";",
    call = NULL
  )
  expect_identical(out[3], "Coefficient C")
  expect_identical(out[5], "Read B from file X header \"B\"")
})

test_that("a partial Read of a non-mapping aborts and a quantified full Read passes (R1-B7)", {
  decl <- "Coefficient (all,r,REG) V(r)"
  expect_null(.chk_raw_reads(c(decl, "Read (all,r,REG) V(r) from file F header \"VV\""), call = NULL))
  expect_snapshot_error(.chk_raw_reads(c(decl, "Read (all,r,REG) V(r,\"usa\") from file F header \"VV\""), call = NULL))
  expect_snapshot_error(.chk_raw_reads(c(decl, "Read (all,r,SUBREG) V(r) from file F header \"VV\""), call = NULL))
  txt <- paste0(
    base_txt,
    "\nCoefficient (all,r,REG) VQ(r);",
    "\nRead (all,r,REG) VQ(r) from file GTAPDATA header \"VQ\";\n"
  )
  model <- process(write_tab(txt))
  rd <- model[model$type == "Read" & model$name == "VQ", ]
  expect_identical(rd$file, "GTAPDATA")
  expect_true(is.na(rd$comp1))
})

test_that("a Read whose file name contains header or from file parses (R1b-B7)", {
  txt <- paste0(
    base_txt,
    "\nFile MYHEADERS;",
    "\nCoefficient AH;",
    "\nRead AH from file MYHEADERS header \"AAAA\";\n"
  )
  model <- process(write_tab(txt))
  rd <- model[model$type == "Read" & model$name == "AH", ]
  expect_identical(rd$file, "MYHEADERS")
  expect_identical(rd$header, "AAAA")
  expect_snapshot_error(.parse_tab_read(
    tibble::tibble(type = "Read", remainder = "AH from file MYHEADERS", row_id = 1L),
    call = NULL
  ))
})

test_that("backsolve rules compare index names case-insensitively (R2-B1)", {
  txt <- paste0(
    base_txt,
    "\nVariable (all,c,COMM) xb(c);",
    "\nEquation E_xb (all,c,COMM) xb(c) = 3*pfactwld;",
    "\nVariable (all,I,COMM) pz(I);",
    "\nEquation E_pz (all,I,COMM) pz(i) = 2*xb(I);",
    "\nBacksolve xb using E_xb;\n"
  )
  model <- process(write_tab(txt))
  expect_identical(model$condense[model$type == "Variable" & model$name == "xb"], "backsolve")
  expect_match(model$tab[model$type == "Equation" & model$name == "E_pz"], "= 2*3*pfactwld", fixed = TRUE)
})

test_that("a set-builder bound index written in another case is rewritten to its indicator (R2-B3)", {
  txt <- paste0(
    base_txt,
    "\nCoefficient (all,c,COMM) XK(c);",
    "\nFormula (all,c,COMM) XK(c) = 1;",
    "\nSet T8 = (all,c,COMM: sum{k,COMM, XK(K)} > 0.5);\n"
  )
  model <- process(write_tab(txt))
  expect_match(model$tab[model$type == "Set" & model$name == "T8"], "SBI01(c) > 0.5", fixed = TRUE)
})
