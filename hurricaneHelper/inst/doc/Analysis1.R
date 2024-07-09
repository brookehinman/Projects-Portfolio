## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(hurricaneHelper)
data(hurdat)

hurdat$year <- as.integer(substr(hurdat$date, 1, 4))

hurdat_2020 <- hurdat[hurdat$year == 2020, ]
ids_2020 <- unique(hurdat_2020$id)

hurdat_2021 <- hurdat[hurdat$year == 2021, ]
ids_2021 <- unique(hurdat_2021$id)

hurdat_2022 <- hurdat[hurdat$year == 2022, ]
ids_2022 <- unique(hurdat_2022$id)

plot_2020 <- plotstorms(ids_2020, hurdat)
plot_2020 <- plot_2020 + ggtitle("Tracks of 2020 Storms") + scale_x_continuous(limits = c(-150, 50))
plot_2020

plot_2021 <- plotstorms(ids_2021, hurdat)
plot_2021 <- plot_2021 + ggtitle("Tracks of 2021 Storms") + scale_x_continuous(limits = c(-150, 50))
plot_2021

plot_2022 <- plotstorms(ids_2022, hurdat)
plot_2022 <- plot_2022 + ggtitle("Tracks of 2022 Storms") + scale_x_continuous(limits = c(-150, 50))
plot_2022


