## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(hurricaneHelper)
data(hurdat)

hurdat$year <- as.numeric(substr(hurdat$date, 1, 4))

# Subset the dataset to include only North Atlantic tropical cyclones
north_atlantic <- subset(hurdat, hurdat$longitude >= -100 &
                             hurdat$longitude <= -20 &
                             hurdat$latitude >= 0 &
                             hurdat$latitude <= 60)

# Calculate the average latitude of maximum intensity for each year
lat_by_year <- aggregate(north_atlantic$latitude, list(north_atlantic$year), max)

# Rename the columns in the aggregate output
names(lat_by_year) <- c("Year", "Avg_Latitude")

# Fit a linear regression model to the average latitude of maximum intensity
# and year, where year is measured in years since 1980
model <- lm(Avg_Latitude ~ Year, data = lat_by_year)

# View the summary of the model
summary(model)

# The estimate found by the model is 0.042412. This means that, on average, the
# latitude of maximum intensity has shifted northward by an average of 0.042412 
# degrees per year since 1851. The p-value is 3.695e-06. Thus, if we use a 
# significance level of 0.05, this means that the relationship if significantly
# significant, providing evidence that the maximum intensity of hurricanes is 
# shifting northward.

