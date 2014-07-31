

context("Blood Volume Calculation by Nadler method")

#TODO: common tests for all functions with invalid, severely out-of-range inputs


test_that("nadlerBloodVolume", {
  expect_error(nadlerBloodVolume())
  expect_error(nadlerBloodVolume(heightm = 1))
  expect_error(nadlerBloodVolume(weightkg = 50))
  expect_error(nadlerBloodVolume(weightkg = 50, male = T))
  expect_error(nadlerBloodVolume(heightm = 1, weightkg=50))
  expect_error(nadlerBloodVolume(heightm = 0, weightkg=50, male = T))
  expect_error(nadlerBloodVolume(heightm = 1, weightkg=5000, male = T))
  expect_error(nadlerBloodVolume(heightm = -1, weightkg=50, male = T))
  expect_error(nadlerBloodVolume(heightm = 1, weightkg=-50, male = T))
  expect_error(nadlerBloodVolume(heightm = 1, weightkg=50, male=""))
  expect_error(nadlerBloodVolume(heightm = 1, weightkg=50, male="xebec"))
  expect_error(nadlerBloodVolume(heightm = 1, weightkg=50, male=list("nonsense",4)))
  expect_false(nadlerBloodVolume(heightm = 1, weightkg=50, male=F) == nadlerBloodVolume(heightm = 1, weightkg=50, male = T))
  h<-c(1,1.5,2)
  w<-c(60,70,80)
  s<-c(FALSE, FALSE, TRUE)
  r<-nadlerBloodVolume(h,w,s)
  expect_equal( nadlerBloodVolume(h[1],w[1],s[1]), r[1] )
  expect_equal( nadlerBloodVolume(h[2],w[2],s[2]), r[2] )
  expect_equal( nadlerBloodVolume(h[3],w[3],s[3]), r[3] )

})

test_that("idealWeight", {
  inch=100/2.54
  expect_error(idealWeight(male = T))
  expect_error(idealWeight(heightm = 1.7))

  expect_warning(idealWeight(heightm = 0, male = T)) # should warn when height is out of validated range of the formula
  expect_warning(idealWeight(heightm=-1, male = T)) # should warn when height is out of validated range of the formula
  expect_warning(idealWeight(heightm=3, male = T)) # should warn when height is out of validated range of the formula
  expect_warning(idealWeight(heightm=59/inch, male = T)) # should warn when height is out of validated range of the formula


  expect_equal(idealWeight(60/inch, male = T), 50)
  expect_equal(idealWeight(60/inch, male = F), 45.5)
  expect_equal(idealWeight(c(60/inch, 60/inch), male=c(F,T)), c(45.5, 50))
  expect_equal(idealWeight(c(60/inch, 60/inch, NA), male=c(F,T,T)), c(45.5, 50, NA))
  expect_equal(idealWeight(c(60/inch, 60/inch, 60/inch), male = c(F,NA,T)), c(45.5, NA, 50))
  expect_error(idealWeight(c(60/inch, 60/inch, 60/inch), male = c(F,T)))
  expect_error(idealWeight(c(60/inch, 60/inch), male = c(F,T,T)))
  expect_error(idealWeight(c(), male=c(F,T,T)))
  expect_error(idealWeight(c(60/inch, 60/inch), male = c()))

  expect_warning(idealWeight(12*8.1/inch, male = T))

})
