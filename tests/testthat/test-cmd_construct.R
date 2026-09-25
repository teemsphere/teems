skip_on_cran()

# The docker --mount value carries the user's own deploy path, so it is
# quoted for the shell system() hands the command to. That quoting is a
# real, intended platform difference: the snapshots normalise it so one
# set serves both platforms, and this test is where the difference is
# pinned instead.
test_that("the mount value is quoted for the platform's shell", {
  spec <- "type=bind,src=/home/u/a b/run,dst=/opt/teems"
  expect_equal(.shell_quote(spec, os = "unix"), sprintf("'%s'", spec))
  expect_equal(.shell_quote(spec, os = "windows"), sprintf('"%s"', spec))

  # a path with a space survives as one argument under either shell
  expect_length(strsplit(.shell_quote(spec, os = "unix"), "'")[[1]], 2L)
  expect_match(.shell_quote(spec, os = "windows"), '^".*"$')

  # the default follows the running platform
  expect_equal(.shell_quote(spec), .shell_quote(spec, os = .Platform$OS.type))
})

test_that("Linux runs the container as the calling user; Docker Desktop hosts do not", {
  flag <- .docker_user_flag(sysname = "Linux")
  ids <- paste0(system2("id", "-u", stdout = TRUE), ":", system2("id", "-g", stdout = TRUE))
  skip_if(!nzchar(ids) || Sys.info()[["sysname"]] != "Linux")
  expect_identical(flag, paste0("--user ", ids, " -e HOME=/tmp "))
  expect_identical(.docker_user_flag(sysname = "Darwin"), "")
  expect_identical(.docker_user_flag(sysname = "Windows"), "")
})
