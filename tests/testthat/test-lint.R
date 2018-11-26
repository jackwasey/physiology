  context("lints")
  test_that("code quality and style", {
    skip_if_not_installed("lintr")
    lintr::expect_lint_free()
  })
