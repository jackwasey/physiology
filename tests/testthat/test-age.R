context("age")

test_that("basic age calcs", {
  expect_equal(
    age_from_dates(birth_date = "2000-01-01",
                   ref_date = "2005-01-01", unit = "year"), 5)
  expect_equal(
    age_from_dates(birth_date = "2000-01-01",
                   ref_date = "2005-01-01", unit = "month"), 60)
  expect_equal(
    age_from_dates(birth_date = "2000-01-01",
                   ref_date = "2000-01-08", unit = "day"), 7)
})
