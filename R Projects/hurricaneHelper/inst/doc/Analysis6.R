## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(hurricaneHelper)
data(hurdat)

hurdat$year <- as.numeric(substr(hurdat$date, 1, 4))

model <- lm(max.wind ~ year, data = hurdat)
summary(model)

