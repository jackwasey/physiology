context("atmospheric pressure")

test_that("human altitudes run", {
  atm_pres(-430.5) # Dead Sea
  atm_pres(0)
  atm_pres(3440) # Namche Bazaar
  atm_pres(4260) # Dingboche
  atm_pres(5364) # Everest Base Camp
  atm_pres(6000) # Camp 1
  atm_pres(6400) # Camp 2
  atm_pres(7200) # Camp 3
  atm_pres(7950) # Camp 4
  atm_pres(8850) # Everest summit
  atm_pres_frac(8850) # fraction of sea level pressure on Everest
})

test_that("high atmosphere", {
  lapply(seq.default(-1000, to = 84000, by = 997),
         function(x) {
           expect_error(regexp = NA, atm_pres_frac(x))
           }
         )
})

test_that("out of upper range", {
  expect_error(regexp = NA, atm_pres(altitude_m = 84849))
  expect_error(atm_pres(altitude_m = 848500))
  expect_error(atm_pres(altitude_m = 1e6))
})
