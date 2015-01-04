context("test util")

test_that("age from dates roughly works", {

  do.age.test <- function() {
    expect_equal(ageFromDates(birthDate = d4, refDate = d3, unit = "year"), 10)
    expect_equal(ageFromDates(birthDate = d4, refDate = d2, unit = "year"), 10)
    expect_equal(ageFromDates(birthDate = d4, refDate = d1, unit = "year"), 10)
    expect_equal(ageFromDates(birthDate = d4, refDate = d0, unit = "year"), 10)

    expect_equal(ageFromDates(birthDate = d3, refDate = d2, unit = "year"), 0)
    expect_equal(ageFromDates(birthDate = d3, refDate = d1, unit = "year"), 0)
    expect_equal(ageFromDates(birthDate = d3, refDate = d0, unit = "year"), 0)

    expect_equal(ageFromDates(birthDate = d2, refDate = d1, unit = "year"), 0)
    expect_equal(ageFromDates(birthDate = d2, refDate = d0, unit = "year"), 0)

    expect_equal(ageFromDates(birthDate = d1, refDate = d0, unit = "year"), 0)

    expect_equal(ageFromDates(birthDate = d0, refDate = d0, unit = "year"), 0)

    expect_equal(ageFromDates(birthDate = d4, refDate = d3, unit = "day"), 3653)
    expect_equal(ageFromDates(birthDate = d4, refDate = d2, unit = "day"), 3804)
    expect_equal(ageFromDates(birthDate = d4, refDate = d1, unit = "day"), 3832)
    expect_equal(ageFromDates(birthDate = d4, refDate = d0, unit = "day"), 3833)

    expect_equal(ageFromDates(birthDate = d3, refDate = d2, unit = "day"), 151)
    expect_equal(ageFromDates(birthDate = d3, refDate = d1, unit = "day"), 179)
    expect_equal(ageFromDates(birthDate = d3, refDate = d0, unit = "day"), 180)

    expect_equal(ageFromDates(birthDate = d2, refDate = d1, unit = "day"), 28)
    expect_equal(ageFromDates(birthDate = d2, refDate = d0, unit = "day"), 29)

    expect_equal(ageFromDates(birthDate = d1, refDate = d0, unit = "day"), 1)

    expect_equal(ageFromDates(birthDate = d0, refDate = d0, unit = "day"), 0)
  }

  d0 <- as.Date("2010-06-30")
  d1 <- as.Date("2010-06-29")
  d2 <- as.Date("2010-06-01")
  d3 <- as.Date("2010-01-01")
  d4 <- as.Date("2000-01-01")
  do.age.test()
  d0 <- "2010-06-30"
  d1 <- "2010-06-29"
  d2 <- "2010-06-01"
  d3 <- "2010-01-01"
  d4 <- "2000-01-01"
  do.age.test()
})
