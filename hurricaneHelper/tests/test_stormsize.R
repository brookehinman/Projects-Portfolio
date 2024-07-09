library(testthat)
library(hurricaneHelper)

test_that("stormsize returns a plot", {
  data(hurdat)
  plot <- stormsize(10, dataf = hurdat)
  expect_true("ggplot" %in% class(plot))
})

test_that("stormsize returns an error for non-numeric rownum input", {
  expect_error(stormsize("a", dataf = hurdat), "Please provide a valid row number within the data frame")
})
