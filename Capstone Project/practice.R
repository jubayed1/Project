library(readxl)
library(tidygeocoder)
library(tidyverse)
library(dplyr)
library(geosphere)
setwd("/Users/abdullahaljubayed/Desktop/Capstone Project")
offender<- read_excel('geocoded_offender.xlsx')
houses_data <- read_excel("warren.xlsx")

house_sold_price <- houses_data %>%
 select("id", "soldprice")

offenders <- offender[, c("lat", 'long')]
houses <- houses_data[, c("latitude", "longitude")]

# Calculate the Haversine distance between two points on the earth's surface
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 # radius of the earth in kilometers
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  a <- sin(dlat/2)^2 + cos(lat1)*cos(lat2)*sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  return(d)
}

# Calculate the distance between each offender and each house
distances <- matrix(nrow = nrow(offenders), ncol = nrow(houses))
for (i in 1:nrow(offenders)) {
  for (j in 1:nrow(houses)) {
    lat1 <- offenders$lat[i]
    lon1 <- offenders$long[i]
    lat2 <- houses$latitude[j]
    lon2 <- houses$longitude[j]
    distances[i, j] <- haversine_distance(lat1, lon1, lat2, lon2)
  }
}

# Convert distances from kilometers to miles
distances_mi <- distances / 1.609344

# Find all offenders that are within 1 mile of each house
offenders_within_1_mile <- apply(distances_mi, 1, function(x) {
  any(x <= 1)
})
view(offenders_within_1_mile)

haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 # radius of the earth in kilometers
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  a <- sin(dlat/2)^2 + cos(lat1)*cos(lat2)*sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  return(d)
}

# Calculate the distance between each offender and each house
distances <- matrix(nrow = nrow(offenders), ncol = nrow(houses))
for (i in 1:nrow(offenders)) {
  for (j in 1:nrow(houses)) {
    lat1 <- offenders$lat[i]
    lon1 <- offenders$long[i]
    lat2 <- houses$latitude[j]
    lon2 <- houses$longitude[j]
    distances[i, j] <- haversine_distance(lat1, lon1, lat2, lon2)
  }
}

# Convert distances from kilometers to miles
distances_mi <- distances / 1.609344

# Find the minimum distance from each offender to any of the houses
min_distances <- apply(distances_mi, 1, min)
min_distances

for (i in 1:nrow(houses)) {
  for (j in 1:nrow(offenders)) {
    distance_in_km <- haversine_distance(houses[i, "latitude"], houses[i, "longitude"],
                                         offenders[j, "lat"], offenders[j, "long"])
    distance_in_miles <- distance_in_km * 0.621371
    
    if (distance_in_miles <= 1) {
      print(sprintf("House %d and offender %d are within 1 mile of each other. Distance: %f miles.", i, j, distance_in_miles))
    }
  }
}
