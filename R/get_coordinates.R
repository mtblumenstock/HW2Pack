#'@title Get Coordinates In Decimal Form
#'
#'@description "get_coordinates" takes a list of latitudes and a list of longitudes in degree/minute format and returns a list of concatenated coordinates in decimal form separated by a delimiter of the user's choice.
#'
#'@param latitudeList a list of latitudes in "xx°yy.zz'N/S/E/W" form
#'@param longitudeList a list of longitudes in "xx°yy.zz'N/S/E/W" form
#'@param delimiter a delimiter that separates the lat and long coordinates in the final list
#'
#'@keywords coordinates latitude longitude north south east west
#'
#'@export
#'
#'@examples
#'get_coordinates("55°44.23'N", "78°23.5'W", ", " )

get_coordinates <- function(latitudeList, longitudeList, delimitor){
  listOfRawCoordinates <- list()
  listOfNewCoordinates <- list()

  for (i in 1:(length(latitudeList))){
    vector <- c(latitudeList[i], longitudeList[i])
    listOfRawCoordinates[[length(listOfRawCoordinates) + 1]] <- vector
  }
  for (j in listOfRawCoordinates){
    latitude <- j[1]
    longitude <- j[2]

    if (grepl("S", latitude) == TRUE){
      latCoeff <- -1
    } else {
      latCoeff <- 1
    }

    if (grepl("W", longitude) == TRUE){
      longCoeff <- -1
    } else {
      longCoeff <- 1
    }

    latitude <- sub(".N", "", latitude)
    latitude <- sub(".S", "", latitude)
    longitude <- sub(".W", "", longitude)
    longitude <- sub(".E", "", longitude)

    latitude <- sub("Ã", "", latitude)
    longitude <- sub("Ã", "", longitude)

    splitLat <- strsplit(latitude, "°")

    latDegrees <- as.numeric(splitLat[[1]][1])

    latNumericMinute <- as.numeric(splitLat[[1]][2])
    latDecimal <- latNumericMinute/60

    latCoordinate <- (latDegrees + latDecimal) * latCoeff

    splitLong <- strsplit(longitude, "°")

    longDegrees <- as.numeric(splitLong[[1]][1])

    longNumericMinute <- as.numeric(splitLong[[1]][2])
    longDecimal <- longNumericMinute/60

    longCoordinate <- (longDegrees + longDecimal) * longCoeff

    latCoordinate <- as.character(latCoordinate)
    longCoordinate <- as.character(longCoordinate)

    newCoordinates <- paste(latCoordinate, longCoordinate, sep = delimitor)

    listOfNewCoordinates[[length(listOfNewCoordinates) + 1]] <- newCoordinates

  }
  return(listOfNewCoordinates)
}
