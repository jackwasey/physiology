context("ventilation")

test_that("anatomic deadspace works", {
  expect_equal(deadspace_anatomic(height_m = 1, age_y = 4), 1)
})
