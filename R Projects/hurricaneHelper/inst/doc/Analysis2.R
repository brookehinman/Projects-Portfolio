## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(hurricaneHelper)
data(hurdat)

# Katrina 2005
plotstorms("AL122005", hurdat)
stormsize(44703, hurdat)

# Sandy 2012
plotstorms("AL182012", hurdat)
stormsize(48707, hurdat)

# Harvey 2017
plotstorms("AL092017", hurdat)
stormsize(50477, hurdat)

# Ian 2022
plotstorms("AL092022", hurdat)
stormsize(53816, hurdat)

