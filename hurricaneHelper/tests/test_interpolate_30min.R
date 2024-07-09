library(testthat)
library(hurricaneHelper)

# Test the interpolate_30min function
test_that("interpolate_30min returns a data.frame", {
  data(hurdat)
  result <- interpolate_30min(hurdat$id[1], hurdat)
  expect_is(result, "data.frame")
})
