#' A function for computing the accumulated cyclone energy of a given storm.
#'
#' This function takes two arguments: a character value representing a storm id and
#' a data frame where the storm information is located.
#'
#' @param storm_id a character value
#' @param data a data frame
#'
#' @return the accumulated storm energy of the chosen storm
#'
#' @examples
#' ACE_calc("AL092004", data = hurdat)
#'
#' @export

ACE_calc <- function(storm_id, data = hurdat){
  # Subset the data based on the track id
  track_data <- subset(data, data$id == storm_id)
  # Use vectorization to compute the ACE
  track_data$ACE <- 10^-4 * track_data$max.wind^2
  ace <- sum(track_data$ACE, na.rm = TRUE)
  return(ace)
}

