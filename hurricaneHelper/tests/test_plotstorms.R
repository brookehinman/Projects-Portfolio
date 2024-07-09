library(testthat)
library(hurricaneHelper)
library(ggplot2)
library(maps)
library(mapdata)
data(hurdat)

# Define test data
test_id_list <- c("AL011851", "AL011852", "AL162003")
test_data <- hurdat

# Test that the function returns a plot object
test_that("plotstorms returns a ggplot object", {
  expect_s3_class(plotstorms(test_id_list, test_data), "ggplot")
})

