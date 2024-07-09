#' A function for interpolating a storm track to 30 minute increments
#'
#' This function takes two arguments: a character value representing a storm id and
#' a data frame where the storm information is located.
#'
#' @param storm_id a character value
#' @param data a data frame
#'
#' @return a data frame with the interpolated data.
#'
#' @examples
#' interpolate_30min("AL092004", data = hurdat)
#'
#' @export

interpolate_30min <- function(track_id, data = hurdat){
  # Subset the data based on the track id
  track_data <- subset(data, data$id == track_id)

  # Convert date and time columns to a datetime column
  track_data$datetime <- as.POSIXct(paste0(track_data$date, sprintf("%04s", track_data$time)), format = "%Y%m%d%H%M")

  # Create a new sequence of datetime values at 30-minute intervals
  new_times <- seq(from = track_data$datetime[1], to = track_data$datetime[nrow(track_data)][1], by = "30 min")

  # Interpolate the hurdat data at the new time intervals, accounting for NA values
  interpolated_data <- data.frame(datetime = new_times)

  # Interpolate numeric variables and add to data frame
  vars_to_interpolate <- c("latitude", "longitude")
  for (var in vars_to_interpolate) {
    if (sum(!is.na(track_data[[var]])) > 1) {
      interpolated_data[[var]] <- approx(track_data$datetime, track_data[[var]], new_times)$y
    }
  }

  # Interpolate the non-numeric variables and add to data frame
  interpolated_data$id <- rep(track_data$id[1], times = nrow(interpolated_data))
  interpolated_data$name <- rep(track_data$name[1], times = nrow(interpolated_data))

  interpolated_data$status <- NA
  for (i in 1:nrow(interpolated_data)) {
    nearest_index <- which.min(abs(track_data$datetime - interpolated_data$datetime[i]))
    interpolated_data$status[i] <- track_data$status[nearest_index]
  }
  return(interpolated_data)
}




