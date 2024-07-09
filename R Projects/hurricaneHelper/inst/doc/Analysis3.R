## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(hurricaneHelper)
data(hurdat)

storms <- split(hurdat, hurdat$id)

storm_data <- data.frame(
  id = character(),
  name = character(),
  max.wind = numeric(),
  min.pressure = numeric(),
  made.landfall = logical(),
  ace = numeric(),
  stringsAsFactors = FALSE
)

for (i in seq_along(storms)){
  id <- storms[[i]]$id[1]
  name <- storms[[i]]$name[1]
  max.wind.total <- max(storms[[i]]$max.wind, na.rm = TRUE)
  min.pressure.total <- min(storms[[i]]$min.pressure, na.rm = FALSE)
  made.landfall <- any(storms[[i]]$identifier == " L")
  ace <- ACE_calc(storms[[i]]$id[1], hurdat)

  storm_data <- rbind(storm_data, data.frame(id = id, name = name, max.wind = max.wind.total,
                                             min.pressure = min.pressure.total,
                                             made.landfall = made.landfall, ace = ace))
}


