test_that("a message with a url closes on a named or a plain link", {
  named <- function() {
    .cli_action("Something needs attention.",
      action = "abort",
      url = "https://teemsphere.github.io/",
      hyperlink = "the teems manual"
    )
  }
  plain <- function() {
    .cli_action("Something needs attention.",
      action = "abort",
      url = "https://teemsphere.github.io/"
    )
  }
  expect_snapshot_error(named())
  expect_snapshot_error(plain())
})
