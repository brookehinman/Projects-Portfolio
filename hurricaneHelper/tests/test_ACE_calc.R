library(testthat)
library(hurricaneHelper)

# Test the ACE_calc function
test_that("ACE_calc returns a numeric value", {
  data(hurdat)
  result <- ACE_calc(hurdat$id[1], hurdat)
  expect_is(result, "numeric")
})

