# function to convert a distance in meters to a degree distance in longitude, FOR A GIVEN LATITUDE #

MetersToDecimalDegrees <- function(meters, latitude) {  # https://stackoverflow.com/questions/25237356/convert-meters-to-decimal-degrees
  return(meters / (111.32 * 1000 * cos(latitude * (pi / 180))));
}