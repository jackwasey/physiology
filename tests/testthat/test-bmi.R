context("Blood Volume Calculation by Nadler method")

#TODO: common tests for all functions with invalid, severely out-of-range inputs

test_that("nadlerBloodVolume", {
  expect_error(nadlerBloodVolume())
  expect_error(nadlerBloodVolume(heightm = 1))
  expect_error(nadlerBloodVolume(weightkg = 50))
  expect_error(nadlerBloodVolume(weightkg = 50, male = TRUE))
  expect_error(nadlerBloodVolume(heightm = 1, weightkg = 50))
  expect_error(nadlerBloodVolume(heightm = 0, weightkg = 50, male = TRUE))
  expect_error(nadlerBloodVolume(heightm = 1, weightkg = 5000, male = TRUE))
  expect_error(nadlerBloodVolume(heightm = -1, weightkg = 50, male = TRUE))
  expect_error(nadlerBloodVolume(heightm = 1, weightkg = -50, male = TRUE))
  expect_error(nadlerBloodVolume(heightm = 1, weightkg = 50, male=""))
  expect_error(nadlerBloodVolume(heightm = 1, weightkg = 50, male="xebec"))
  expect_error(nadlerBloodVolume(heightm = 1, weightkg = 50,
                                 male=list("nonsense", 4)))
  expect_that(nadlerBloodVolume(heightm = 1, weightkg = 50, male=F),
              testthat::not(testthat::equals(
                 nadlerBloodVolume(heightm = 1, weightkg = 50, male = TRUE))))
  h <- c(1, 1.5, 2)
  w <- c(60, 70, 80)
  s <- c(FALSE, FALSE, TRUE)
  r <- nadlerBloodVolume(h, w, s)
  expect_equal(nadlerBloodVolume(h[1], w[1], s[1]), r[1] )
  expect_equal(nadlerBloodVolume(h[2], w[2], s[2]), r[2] )
  expect_equal(nadlerBloodVolume(h[3], w[3], s[3]), r[3] )

})

context("ideal weight")

test_that("idealWeightAdultAdult", {
  inch <- 100 / 2.54
  expect_error(idealWeightAdult(male = TRUE))
  expect_error(idealWeightAdult(heightm = 1.7))

  # should warn when height is out of validated range of the formula
  expect_warning(idealWeightAdult(heightm = 0, male = TRUE))
  # should warn when height is out of validated range of the formula
  expect_warning(idealWeightAdult(heightm = -1, male = TRUE))
  # should warn when height is out of validated range of the formula
  expect_warning(idealWeightAdult(heightm = 3, male = TRUE))
  # should warn when height is out of validated range of the formula
  expect_warning(idealWeightAdult(heightm = 59 / inch, male = TRUE))


  expect_equal(idealWeightAdult(60 / inch, male = TRUE), 50)
  expect_equal(idealWeightAdult(60 / inch, male = F), 45.5)
  expect_equal(idealWeightAdult(c(60 / inch, 60 / inch), male=c(FALSE, TRUE)),
               c(45.5, 50))
  expect_equal(idealWeightAdult(c(60 / inch, 60 / inch, NA),
                                male=c(FALSE, TRUE, TRUE)),
               c(45.5, 50, NA))
  expect_equal(idealWeightAdult(c(60 / inch, 60 / inch, 60 / inch),
                                male = c(FALSE, NA, TRUE)),
               c(45.5, NA, 50))
  expect_error(idealWeightAdult(c(60 / inch, 60 / inch, 60 / inch),
                                male = c(FALSE, TRUE)))
  expect_error(idealWeightAdult(c(60 / inch, 60 / inch),
                                male = c(FALSE, TRUE, TRUE)))
  expect_error(idealWeightAdult(c(), male=c(FALSE, TRUE, TRUE)))
  expect_error(idealWeightAdult(c(60 / inch, 60 / inch), male = c()))

  expect_warning(idealWeightAdult(12 * 8.1 / inch, male = TRUE))

})
