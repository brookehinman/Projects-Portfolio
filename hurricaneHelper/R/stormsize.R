#' A function for plotting a map of a selected storm at one point and arms representing wind traveled in nautical miles from the origin.
#'
#' This function takes two arguments: a row number within the data frame and a data frame
#' where the storm information is located.
#'
#' @param rownum a row number within the data frame
#' @param dataf a data frame
#'
#' @return a plot of the origin and how far the winds have moved in the NE, SE, SW, and NW directions
#'
#' @examples
#' stormsize(52299, dataf = hurdat)
#'
#' @export

stormsize <- function(rownum, dataf = hurdat){

  # Check that row number is valid within data frame
  if(rownum >= nrow(dataf) | rownum <= 0){
    stop("Please provide a valid row number within the data frame.")
  }
  # Get the row from dataframe
  hurdat <- dataf
  onehur <- hurdat[rownum,]

  # Gather information for the storms
  cir1 <- data.frame(NE64 = onehur$NE64, SE64 = onehur$SE64, SW64 = onehur$SW64, NW64 = onehur$NW64)
  cir2 <- data.frame(NE50 = onehur$NE50, SE50 = onehur$SE50, SW50 = onehur$SW50, NW50 = onehur$NW50)
  cir3 <- data.frame(NE34 = onehur$NE34, SE34 = onehur$SE34, SW34 = onehur$SW34, NW34 = onehur$NW34)
  circs <- data.frame(cir1, cir2, cir3)
  circs[circs == 0] <- NA

  # Origin point of storm
  origin <- data.frame(lon = onehur$longitude, lat = onehur$latitude)

  # Empty dataframe to store points of movement
  destinations <- data.frame()

  # Get longitude and latitude points of movement
  iter <-c(1,5,9)
  for (j in iter){
    set <- circs[j:(j+3)]

    #NE
    lat_delta <- set[[1]] / 60
    lon_delta <- set[[1]] / (60 * cos(origin$lat * pi/180))

    # Calculate the new coordinates
    lat <- origin$lat + lat_delta
    lon <- origin$lon + lon_delta

    dest <- data.frame(lon, lat)
    destinations <- rbind(destinations, dest)

    #SE
    lat_delta <- -set[[2]] / 60
    lon_delta <- set[[2]] / (60 * cos(origin$lat * pi/180))

    # Calculate the new coordinates
    lat <- origin$lat + lat_delta
    lon <- origin$lon + lon_delta

    dest <- data.frame(lon, lat)
    destinations <- rbind(destinations, dest)

    #SW
    lat_delta <- -set[[3]] / 60
    lon_delta <- -set[[3]] / (60 * cos(origin$lat * pi/180))

    # Calculate the new coordinates
    lat <- origin$lat + lat_delta
    lon <- origin$lon + lon_delta

    dest <- data.frame(lon, lat)
    destinations <- rbind(destinations, dest)

    #NW
    lat_delta <- set[[4]] / 60
    lon_delta <- -set[[4]] / (60 * cos(origin$lat * pi/180))

    # Calculate the new coordinates
    lat <- origin$lat + lat_delta
    lon <- origin$lon + lon_delta

    dest <- data.frame(lon, lat)
    destinations <- rbind(destinations, dest)

  }

  # Dataframe of origin and destinations
  data <- cbind(origin, destinations)
  data$set_num <- data.frame(set_num = rep(1:3, each = 4))
  colnames(data) <- c("orig_lon", "orig_lat", "dest_lon", "dest_lat", "set_num")

  # Plot storm on world map
  # Get the bounding box of the storm tracks
  bbox <- c(min(onehur$longitude), max(onehur$longitude), min(onehur$latitude), max(onehur$latitude))
  ggplot(data, aes(x = longitude, y = latitude)) +
    borders("world", colour = "black", xlim = c(bbox[1], bbox[2]), ylim = c(bbox[3], bbox[4])) +
    borders("state", colour = "black") +
    geom_point(aes(x = orig_lon, y = orig_lat, color = "origin"), size = 2) +
    geom_point(aes(x = origin$lon, y = origin$lat, color = factor(set_num)), size = 2) +
    geom_curve(data = subset(data, set_num == 1), aes(x = orig_lon, y = orig_lat, xend = dest_lon, yend = dest_lat, color = "64")) +
    geom_curve(data = subset(data, set_num == 2), aes(x = orig_lon, y = orig_lat, xend = dest_lon, yend = dest_lat, color = "50")) +
    geom_curve(data = subset(data, set_num == 3), aes(x = orig_lon, y = orig_lat, xend = dest_lon, yend = dest_lat, color = "34")) +
    ggtitle(paste0("Storm movement of ", onehur$name, " at time ", onehur$time)) +
    scale_color_discrete(name = "Legend")
}


