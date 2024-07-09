#' A function for plotting a map of storm tracks for a selection of storms.
#'
#' This function takes two arguments: a list of storm ids and a data frame
#' where the storm information is located.
#'
#' @param id_list a list of character values representing the storm ids
#' @param data a data frame
#'
#' @return a plot of the storm tracks
#'
#' @import ggplot2
#' @import maps
#' @import mapdata
#' @examples
#' plotstorms(c("AL011851", "AL011852", "AL162003"), data = hurdat)
#'
#' @export

plotstorms <- function(id_list, data = hurdat) {
  # Stop if is_list is not a vector
  if (!is.vector(id_list)) {
    stop("The input argument must be a vector.")
  }

  # Stop if an argument within the vector is not a string
  for (n in 1:length(id_list)) {
    if (!is.character(id_list[n])) {
      stop("Input must be of type character.")
    }
  }

  # Stop function if an input is not a valid ID
  for(L in 1:length(id_list)){
    if (!(id_list[L] %in% data$id)) {
      stop("One or more string inputs not found in id column.")
    }
  }
  # Get the bounding box of the storm tracks
  bbox <- c(min(data$longitude), max(data$longitude), min(data$latitude), max(data$latitude))
  # Initialize world map with country and state borders
  world_map <- ggplot() + borders("world", colour = "black", xlim = c(bbox[1], bbox[2]), ylim = c(bbox[3], bbox[4])) +
    borders("state", colour = "black")
  # Get number of hurricanes
  len <- length(id_list)
  # Generate a sequence of colors
  color <- rainbow(len)
  # Create a data frame that maps each storm ID to a color
  color_df <- data.frame(id = id_list, color = color, stringsAsFactors = FALSE)
  # Plot each hurricane on the same map
  for (i in 1:len) {
    hur1 <- data[data$id == id_list[i], ]
    hurlat <- hur1$latitude
    hurlong <- hur1$longitude
    # Create a data frame of coordinates
    coords <- data.frame(lon = hurlong, lat = hurlat, id = id_list[i])
    # Plot the coordinates on the world map
    # The stronger the wind speed, the bigger the points
    world_map <- world_map + geom_point(data = coords, aes(x = lon, y = lat, color = id), size = hur1$max.wind/100) +
      coord_fixed()
  }
  # Add legend to the map
  world_map <- world_map +
    scale_color_manual(values = color_df$color, name = "Storm ID:", labels = color_df$id) +
    theme(legend.position = "bottom")
  # Return the world map with all hurricanes plotted
  return(world_map)
}



