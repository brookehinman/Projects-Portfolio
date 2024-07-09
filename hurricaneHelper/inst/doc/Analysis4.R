## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(hurricaneHelper)
data(hurdat)


# Time series of hurricane frequency
hurdat$year <- as.numeric(substr(hurdat$date, 1, 4))
hurdat$month <- as.numeric(substr(hurdat$date, 5, 6))

# Count number of hurricanes per year
hurricanes_per_year <- aggregate(hurdat$year, by = list(hurdat$year), FUN = length)

# Create time series plot
ggplot(hurricanes_per_year, aes(x = Group.1, y = x)) +
  geom_line() +
  xlab("Year") +
  ylab("Number of hurricanes")

model <- lm(x ~ Group.1, data = hurricanes_per_year)
summary(model)


# Histogram of Max Wind Speeds
ggplot(hurdat, aes(x = max.wind)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.5) +
  labs(title = "Distribution of Maximum Wind Speeds", x = "Maximum Wind Speed", y = "Count") +
  theme_minimal()

