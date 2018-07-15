context("acid base")

test_that("Henderson Haselbach", {
  expect_equal(henderson_hasselbach(25, 40), 7.4, tolerance = 0.01)
})
