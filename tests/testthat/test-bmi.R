context("ideal weight")

#TODO: common tests for all functions with invalid, severely out-of-range inputs

test_that("ideal_weight_adultAdult", {
  inch <- 100 / 2.54
  expect_error(ideal_weight_adult(male = TRUE))
  expect_error(ideal_weight_adult(heightm = 1.7))

  # should warn when height is out of validated range of the formula
  expect_warning(ideal_weight_adult(heightm = 0, male = TRUE, do.warn = TRUE))
  # should warn when height is out of validated range of the formula
  expect_warning(ideal_weight_adult(heightm = -1, male = TRUE, do.warn = TRUE))
  # should warn when height is out of validated range of the formula
  expect_warning(ideal_weight_adult(heightm = 3, male = TRUE, do.warn = TRUE))
  # should warn when height is out of validated range of the formula
  #   expect_warning(ideal_weight_adult(heightm = 59 / inch,
  #                                     male = TRUE,
  #                                     do.warn = TRUE))
  expect_warning(ideal_weight_adult(heightm = 59 / inch,
                                 male = TRUE,
                                 do.warn = FALSE), NA)

  expect_equal(ideal_weight_adult(60 / inch, male = TRUE), 50)
  expect_equal(ideal_weight_adult(60 / inch, male = F), 45.5)
  expect_equal(ideal_weight_adult(heightm = c(60 / inch, 60 / inch),
                                  male = c(FALSE, TRUE)),
               c(45.5, 50))
  expect_equal(ideal_weight_adult(c(60 / inch, 60 / inch, NA),
                                  male = c(FALSE, TRUE, TRUE)),
               c(45.5, 50, NA))
  expect_equal(ideal_weight_adult(c(60 / inch, 60 / inch, 60 / inch),
                                  male = c(FALSE, NA, TRUE)),
               c(45.5, NA, 50))
  expect_error(ideal_weight_adult(c(60 / inch, 60 / inch, 60 / inch),
                                  male = c(FALSE, TRUE)))
  expect_error(ideal_weight_adult(c(60 / inch, 60 / inch),
                                  male = c(FALSE, TRUE, TRUE)))
  expect_error(ideal_weight_adult(c(), male = c(FALSE, TRUE, TRUE)))
  expect_error(ideal_weight_adult(c(60 / inch, 60 / inch), male = c()))

  expect_warning(ideal_weight_adult(12 * 8.5 / inch, male = TRUE, do.warn = TRUE))

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
  expect_warning(ideal_weight_Straub(-1, age.years = 10, do.warn = FALSE))
  expect_warning(ideal_weight_Straub(5, age.years = 10, do.warn = FALSE))
  expect_warning(ideal_weight_Straub(1, age.years = -1, do.warn = FALSE))
  expect_warning(ideal_weight_Straub(1, age.years = 200, do.warn = FALSE))
  expect_warning(ideal_weight_Straub(1, age.months = 200 * 12, do.warn = FALSE))
  expect_warning(ideal_weight_Straub(1, age.days = 200 * 365, do.warn = FALSE))

  # optionally warn for unvalidated but possible ages
  expect_warning(ideal_weight_Straub(1, age.years = 0.5, do.warn = TRUE))
  expect_warning(ideal_weight_Straub(1, age.months = 11, do.warn = TRUE))
  expect_warning(ideal_weight_Straub(1, age.days = 350, do.warn = TRUE))

  expect_warning(ideal_weight_Straub(1, age.years = 19, do.warn = TRUE))
  expect_warning(ideal_weight_Straub(1, age.months = 18 * 12 + 1, do.warn = TRUE))
  expect_warning(ideal_weight_Straub(1, age.days = 18 * 366, do.warn = TRUE))

  expect_warning(ideal_weight_Straub(1, age.years = 0.5, do.warn = FALSE), NA)
  expect_warning(ideal_weight_Straub(1, age.months = 11, do.warn = FALSE), NA)
  expect_warning(ideal_weight_Straub(1, age.days = 350, do.warn = FALSE), NA)
  expect_warning(ideal_weight_Straub(1, age.years = 18, do.warn = FALSE), NA)
  expect_warning(ideal_weight_Straub(1, age.months = 18 * 12 + 1, do.warn = FALSE), NA)
  expect_warning(ideal_weight_Straub(1, age.days = 18 * 366, do.warn = FALSE), NA)

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

test_that("child ideal weight defaults to Straub", {
  expect_equal(ideal_weight_child(1, age.days = 700),
               ideal_weight_Straub(1, age.days = 700))
})

test_that("body surface area", {
  expect_equal(bsa_adult(heightm = 2, weightkg = 72), 2)
  expect_equal(bsa_adult(heightm = c(NA, 2), weightkg = c(70, 72)), c(NA_real_, 2))
})

test_that("ideal weight Lemmens", {
  expect_equal(ideal_weight_Lemmens(2), 88)
})

test_that("BMI adult", {
  expect_equal(bmi_adult(2, 100), 25)
})

test_that("BMI adult imperial measure", {
  expect_equal(bmi_adult_ins_lbs(72, 200), 27.124600709)
})

test_that("blood vol Lemmens not obese", {
  expect_error(blood_vol_Lemmens_non_obese())
  expect_error(blood_vol_Lemmens_non_obese(NA))
  expect_error(blood_vol_Lemmens_non_obese(NA, NA))
  expect_error(blood_vol_Lemmens_non_obese(NULL))
  expect_error(blood_vol_Lemmens_non_obese(NULL, NULL))
  expect_error(blood_vol_Lemmens_non_obese(NULL, NULL, NULL))
  expect_error(blood_vol_Lemmens_non_obese(50))
  expect_error(blood_vol_Lemmens_non_obese(weightkg = 50))
  expect_error(blood_vol_Lemmens_non_obese(age = 20))
  expect_error(blood_vol_Lemmens_non_obese(male = TRUE))
  expect_error(blood_vol_Lemmens_non_obese(weightkg = 50, age = 20))
  expect_error(blood_vol_Lemmens_non_obese(age = 20, male = FALSE))
  expect_error(blood_vol_Lemmens_non_obese(weightkg = 50, male = TRUE))

  expect_error(blood_vol_Lemmens_non_obese(weightkg = 50, age = 50, male = "M"))

  expect_error(blood_vol_Lemmens_non_obese(weightkg = 50, age = 50, male = c(TRUE, FALSE)))
  expect_error(blood_vol_Lemmens_non_obese(weightkg = 50, age = c(50, 60), male = FALSE))
  expect_error(blood_vol_Lemmens_non_obese(weightkg = c(50, 75), age = 50, male = FALSE))

  expect_equal(blood_vol_Lemmens_non_obese(50, 50, TRUE), 3500)
  expect_equal(blood_vol_Lemmens_non_obese(50, 50, FALSE), 3250)
})

test_that("height, weight funcs invalid input", {
  funs <- c("bsa_adult", "adj_weight", "adj_weight_adult",
            "blood_vol_Lemmens_indexed", "blood_vol_Lemmens_sedentary",
            "blood_vol_Nadler", "bmi_adult", "adj_weight_adult")

  # now give conditions that should be true for all these functions:
  for (f in funs) {
    expect_error(do.call(f), info = f)
    expect_error(do.call(f, bad_input), info = f)
    expect_error(do.call(f, list(heightm = 1.5)), info = f)
    expect_error(do.call(f, list(weightkg = 40)), info = f)
    expect_error(do.call(f, list(NA_real_)), info = f)
    expect_error(do.call(f, list(NULL)), info = f)
    expect_error(do.call(f, list(NULL, NULL)), info = f)

    #mismatch length
    expect_error(do.call(f, list(heightm = c(1.5, 2), weightkg = 40)), info = f)
    expect_error(do.call(f, list(heightm = 2, weightkg = c(40, 50))), info = f)
    expect_error(do.call(f, list(heightm = c(1.5, 2), weightkg = NULL)), info = f)
    expect_error(do.call(f, list(heightm = NULL, weightkg = c(40, 50))), info = f)
    expect_error(do.call(f, list(heightm = c(1.5, NA), weightkg = 40)), info = f)
    expect_error(do.call(f, list(heightm = 2, weightkg = c(NA, 50))), info = f)
    expect_error(do.call(f, list(heightm = NULL, weightkg = 40)), info = f)
    expect_error(do.call(f, list(heightm = 2, weightkg = NULL)), info = f)

    # stop/warn conditions: we don't know that each specific function doesn't have
    # tighter rules or other arguments required, so we can only look for errors
    # here, if we test all ht/wt functions together.
    expect_error(do.call(f, list(heightm = -1, weightkg = 50, do.stop = TRUE)), info = f)
    expect_error(do.call(f, list(heightm =  0, weightkg = 50, do.stop = TRUE)), info = f)
    expect_error(do.call(f, list(heightm = 1.5, weightkg = -50, do.stop = TRUE)), info = f)
    expect_error(do.call(f, list(heightm = 1.5, weightkg =   0, do.stop = TRUE)), info = f)
    expect_error(do.call(f, list(heightm = 8, weightkg = 50, do.stop = TRUE)), info = f)
    expect_error(do.call(f, list(heightm = 1.5, weightkg = 5000, do.stop = TRUE)), info = f)
  }
})

test_that("height only funcs invalid input", {
  funs <- c("ideal_weight_Lemmens")

  # now give conditions that should be true for all these functions:
  for (f in funs) {
    expect_error(do.call(f), info = f)
    expect_error(do.call(f, bad_input), info = f)
    expect_error(do.call(f, list(NULL)), info = f)
    expect_error(do.call(f, list(NULL, NULL)), info = f)

    # stop/warn conditions: we don't know that each specific function doesn't have
    # tighter rules or other arguments required, so we can only look for errors
    # here, if we test all ht/wt functions together.
    expect_error(do.call(f, list(heightm = -1, do.stop = TRUE)), info = f)
    expect_error(do.call(f, list(heightm =  0, do.stop = TRUE)), info = f)
    expect_error(do.call(f, list(heightm = 8, do.stop = TRUE)), info = f)
  }
})

test_that("height, gender funcs invalid input", {
  funs <- c("ideal_weight_adult", "ideal_weight_Broca",
            "ideal_weight_Devine", "ideal_weight_Miller",
            "ideal_weight_Robinson"
  )

  # now give conditions that should be true for all these functions:
  for (f in funs) {
    expect_error(do.call(f), info = f)
    expect_error(do.call(f, bad_input), info = f)
    expect_error(do.call(f, list(heightm = 1.5)), info = f)
    expect_error(do.call(f, list(male = T)), info = f)
    expect_error(do.call(f, list(NA_real_)), info = f)
    expect_error(do.call(f, list(NULL)), info = f)
    expect_error(do.call(f, list(NULL, NULL)), info = f)

    #mismatch length
    expect_error(do.call(f, list(heightm = c(1.5, 2), male = T)), info = f)
    expect_error(do.call(f, list(heightm = 2, male = c(T, F))), info = f)
    expect_error(do.call(f, list(heightm = c(1.5, 2), male = NULL)), info = f)
    expect_error(do.call(f, list(heightm = NULL, male = c(T, F))), info = f)
    expect_error(do.call(f, list(heightm = c(1.5, NA), male = T)), info = f)
    expect_error(do.call(f, list(heightm = 2, male = c(NA, T))), info = f)
    expect_error(do.call(f, list(heightm = NULL, male = F)), info = f)
    expect_error(do.call(f, list(heightm = 2, male = NULL)), info = f)

    # stop/warn conditions: we don't know that each specific function doesn't have
    # tighter rules or other arguments required, so we can only look for errors
    # here, if we test all ht/wt functions together.
    expect_error(do.call(f, list(heightm = -1, male = T, do.stop = TRUE)), info = f)
    expect_error(do.call(f, list(heightm =  0, male = T, do.stop = TRUE)), info = f)
    expect_error(do.call(f, list(heightm = 1.5, male = "m", do.stop = TRUE)), info = f)
    expect_error(do.call(f, list(heightm = 1.5, male = 2L, do.stop = TRUE)), info = f)
    expect_error(do.call(f, list(heightm = 1.5, male = -1, do.stop = TRUE)), info = f)
    expect_error(do.call(f, list(heightm = 8, male = T, do.stop = TRUE)), info = f)
  }
})
