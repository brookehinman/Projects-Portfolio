#' A function for determining whether a storm made landfall in the continental US
#'
#' This function takes two arguments: a character value representing a storm id and
#' a data frame where the storm information is located.
#'
#' @param storm_id a character value
#' @param data a data frame
#'
#' @return a logical vector indicating whether the storm made landfall or not
#'
#' @examples
#' is_US_landfall("AL092004", data = hurdat)
#'
#' @export

is_US_landfall <- function(storm_id, data = hurdat) {
  storm_data <- subset(data, data$id == storm_id)
  library(maps)
  library(sp)
  # Extract the storm's latitude and longitude values
  storm_lat <- storm_data$latitude
  storm_lon <- storm_data$longitude

  # Get the boundaries for the continental US using the maps package
  us_boundaries <- map("state", plot = FALSE)

  # Combine x and y coordinates into a matrix, removing any rows with NA values
  us_coords <- na.omit(cbind(us_boundaries$x, us_boundaries$y))

  # Create a polygon object from the US boundaries
  us_polygon <- Polygon(coords = us_coords, hole = FALSE)
  us_polygons <- Polygons(list(us_polygon), ID = "US")
  us_sp <- SpatialPolygons(list(us_polygons))

  # Check if any of the storm data points fall within the US polygon using point.in.polygon
  for (i in 1:length(storm_lat)) {
    lat <- storm_lat[i]
    lon <- storm_lon[i]
    if (point.in.polygon(lon, lat, us_boundaries$x, us_boundaries$y)) {
      return(TRUE)
    }
  }

  # If no storm data points fall within the US polygon, return FALSE
  return(FALSE)
}


