context("ideal weight")

#TODO: common tests for all functions with invalid, severely out-of-range inputs

test_that("ideal_weight_adultAdult", {
  inch <- 100 / 2.54
  expect_error(ideal_weight_adult(male = TRUE))
  expect_error(ideal_weight_adult(heightm = 1.7))

  # should warn when height is out of validated range of the formula
  expect_warning(ideal_weight_adult(heightm = 0, male = TRUE, warn = TRUE))
  # should warn when height is out of validated range of the formula
  expect_warning(ideal_weight_adult(heightm = -1, male = TRUE, warn = TRUE))
  # should warn when height is out of validated range of the formula
  expect_warning(ideal_weight_adult(heightm = 3, male = TRUE, warn = TRUE))
  # should warn when height is out of validated range of the formula
  expect_warning(ideal_weight_adult(heightm = 59 / inch,
                                    male = TRUE,
                                    warn = TRUE))
  expect_that(ideal_weight_adult(heightm = 59 / inch,
                                 male = TRUE,
                                 warn = FALSE),
              testthat::not(gives_warning()))


  expect_equal(ideal_weight_adult(60 / inch, male = TRUE), 50)
  expect_equal(ideal_weight_adult(60 / inch, male = F), 45.5)
  expect_equal(ideal_weight_adult(c(60 / inch, 60 / inch), male=c(FALSE, TRUE)),
               c(45.5, 50))
  expect_equal(ideal_weight_adult(c(60 / inch, 60 / inch, NA),
                                  male=c(FALSE, TRUE, TRUE)),
               c(45.5, 50, NA))
  expect_equal(ideal_weight_adult(c(60 / inch, 60 / inch, 60 / inch),
                                  male = c(FALSE, NA, TRUE)),
               c(45.5, NA, 50))
  expect_error(ideal_weight_adult(c(60 / inch, 60 / inch, 60 / inch),
                                  male = c(FALSE, TRUE)))
  expect_error(ideal_weight_adult(c(60 / inch, 60 / inch),
                                  male = c(FALSE, TRUE, TRUE)))
  expect_error(ideal_weight_adult(c(), male=c(FALSE, TRUE, TRUE)))
  expect_error(ideal_weight_adult(c(60 / inch, 60 / inch), male = c()))

  expect_warning(ideal_weight_adult(12 * 8.1 / inch, male = TRUE, warn = TRUE))

})

test_that("Straub", {
  expect_error(ideal_weight_Straub(bad_input))
  expect_error(ideal_weight_Straub(bad_input, bad_input))
  expect_error(ideal_weight_Straub())

  # spec age correctly
  expect_error(ideal_weight_Straub(1, age.years = 2, age.months = 2))
  expect_error(ideal_weight_Straub(1, age.years = 2, age.days = 100))
  expect_error(ideal_weight_Straub(1, age.months = 50, age.days = 100))

  # definitely invalid numbers always warn:
  expect_warning(ideal_weight_Straub(-1, age.years = 10, warn = FALSE))
  expect_warning(ideal_weight_Straub(5, age.years = 10, warn = FALSE))
  expect_warning(ideal_weight_Straub(1, age.years = -1, warn = FALSE))
  expect_warning(ideal_weight_Straub(1, age.years = 200, warn = FALSE))
  expect_warning(ideal_weight_Straub(1, age.months = 200 * 12, warn = FALSE))
  expect_warning(ideal_weight_Straub(1, age.days = 200 * 365, warn = FALSE))

  # optionally warn for unvalidated but possible ages
  expect_that(ideal_weight_Straub(1, age.years = 0, warn = TRUE),
              gives_warning())
  expect_that(ideal_weight_Straub(1, age.months = 11, warn = TRUE),
              gives_warning())
  expect_that(ideal_weight_Straub(1, age.days = 350, warn = TRUE),
              gives_warning())

  expect_that(ideal_weight_Straub(1, age.years = 18, warn = TRUE),
              gives_warning())
  expect_that(ideal_weight_Straub(1, age.months = 18 * 12 + 1, warn = TRUE),
              gives_warning())
  expect_that(ideal_weight_Straub(1, age.days = 18 * 366, warn = TRUE),
              gives_warning())

  expect_that(ideal_weight_Straub(1, age.years = 0, warn = FALSE),
              testthat::not(gives_warning()))
  expect_that(ideal_weight_Straub(1, age.months = 11, warn = FALSE),
              testthat::not(gives_warning()))
  expect_that(ideal_weight_Straub(1, age.days = 350, warn = FALSE),
              testthat::not(gives_warning()))

  expect_that(ideal_weight_Straub(1, age.years = 18, warn = FALSE),
              testthat::not(gives_warning()))
  expect_that(ideal_weight_Straub(1, age.months = 18 * 12 + 1, warn = FALSE),
              testthat::not(gives_warning()))
  expect_that(ideal_weight_Straub(1, age.days = 18 * 366, warn = FALSE),
              testthat::not( gives_warning()))

  # don't need to specify age, because it is not used in calc, just validation
  expect_that(ideal_weight_Straub(1),
              testthat::equals(2.396^1.863))
  expect_that(ideal_weight_Straub(1, age.years = 3),
              testthat::equals(2.396^1.863))
  expect_that(ideal_weight_Straub(1, age.months = 20),
              testthat::equals(2.396^1.863))
  expect_that(ideal_weight_Straub(1, age.days = 700),
              testthat::equals(2.396^1.863))

})
