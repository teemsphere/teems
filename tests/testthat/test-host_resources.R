# Container inspection for resources = "auto" and the memory checks:
# the parser on canned output, and the live probe where docker exists.

test_that("container inspection output is parsed", {
  h <- .parse_container_resources(c("8", "12884901888", "MemTotal:       16337108 kB"))
  expect_identical(h$cores, 8L)
  expect_equal(h$mem_gb, 12.884901888)
  expect_equal(h$cgroup_limit_gb, 12.884901888)
  expect_equal(h$mem_total_gb, 16337108 * 1024 / 1e9)
  # cgroup v2 without a limit
  h <- .parse_container_resources(c("32", "max", "MemTotal: 131072000 kB"))
  expect_true(is.na(h$cgroup_limit_gb))
  expect_equal(h$mem_gb, 131072000 * 1024 / 1e9)
  # cgroup v1 without a limit reports a huge number
  h <- .parse_container_resources(c("4", "9223372036854771712", "MemTotal: 8000000 kB"))
  expect_true(is.na(h$cgroup_limit_gb))
  expect_equal(h$mem_gb, 8000000 * 1024 / 1e9)
  # a limit above the VM's memory is the VM's memory
  h <- .parse_container_resources(c("4", "64000000000", "MemTotal: 8000000 kB"))
  expect_equal(h$mem_gb, 8000000 * 1024 / 1e9)
  # blank lines are dropped, garbage is NULL
  expect_identical(.parse_container_resources(c("", "8", "max", "MemTotal: 1 kB"))$cores, 8L)
  expect_null(.parse_container_resources(character()))
  expect_null(.parse_container_resources(c("x", "y", "z")))
  expect_null(.parse_container_resources(c("0", "max", "MemTotal: 1 kB")))
})

test_that("the live inspection reports this machine's container once per session", {
  skip_if_not(nzchar(Sys.which("docker")))
  image <- paste0("teems:", .resolve_docker_tag(quiet = TRUE))
  h <- .container_resources(image, refresh = TRUE)
  skip_if(is.null(h))
  expect_gte(h$cores, 1L)
  expect_gt(h$mem_gb, 0)
  expect_identical(h$image, image)
  expect_identical(.container_resources(image), h)
})
