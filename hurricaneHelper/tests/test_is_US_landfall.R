library(testthat)
library(hurricaneHelper)

# Test the is_US_landfall function
test_that("is_US_landfall returns a logical value", {
  data(hurdat)
  result <- is_US_landfall(hurdat$id[1], hurdat)
  expect_is(result, "logical")
})
