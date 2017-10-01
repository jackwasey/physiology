context("ventilation")

test_that("anatomic deadspace works", {

  #test age over 6 and under 6.
  expect_equal(
    deadspace_anatomic_child(ideal_weight_kg = 10),
    deadspace_anatomic_child(ideal_weight_kg = 10, age_y = 6)
  )

  expect_equal(
    deadspace_anatomic_child(ideal_weight_kg = 10, age_y = 6),
    deadspace_anatomic_child(ideal_weight_kg = 10, age_y = 7)
  )

  expect_equal(deadspace_anatomic_child(ideal_weight_kg = 10), 22)

  expect_equal(deadspace_anatomic_child(ideal_weight_kg = 10, age_y = 5), 22.76615, tolerance = 1e-5)

})
