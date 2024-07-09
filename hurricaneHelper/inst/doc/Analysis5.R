## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(hurricaneHelper)
data(hurdat)

hurdat$year <- as.numeric(substr(hurdat$date, 1, 4))

landfalls <- hurdat[hurdat$identifier == " L", ]

landfalls_unique <- landfalls[!duplicated(landfalls$id), ]

plot(table(landfalls_unique$year), type = "b", xlab = "Year", ylab = "Number of landfalling hurricanes", main = "Number of landfalling hurricanes over time")

# Statistical test
landfalls_per_year <- as.data.frame(table(landfalls_unique$year))
names(landfalls_per_year) <- c("Year", "Landfall_Count")


# Create a sequence of years
all_years <- data.frame(Year = min(landfalls_unique$year):max(landfalls_unique$year))

# Merge with landfalls_per_year using a left join
landfalls_all_years <- merge(all_years, landfalls_per_year, by = "Year", all.x = TRUE)

# Replace missing landfall counts with 0
landfalls_all_years$Landfall_Count[is.na(landfalls_all_years$Landfall_Count)] <- 0

# Fit the linear regression model
model <- lm(Landfall_Count ~ Year, data = landfalls_all_years)
summary(model)

