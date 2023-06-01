library(readxl)
library(tidygeocoder)
library(tidyverse)
library(dplyr)
library(geosphere)
#install.packages('geosphere')
 #housedata <- read_excel("warren.xlsx")

setwd("/Users/abdullahaljubayed/Desktop/Capstone Project")
offenders<- read_excel('geocoded_offender.xlsx')

haversine <- function(lat1, lon1, lat2, lon2) {
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
  c <- 2 * asin(min(1, sqrt(a)))
  R <- 3961 # radius of earth in miles
  d <- R * c
  return(d)
}

# Data frame of house locations
houses_data <- read_excel("warren.xlsx")


# Extract the latitude and longitude columns into a new data frame
houses <- houses_data[, c("latitude", "longitude")]

# Data frame of sex offender locations
sex_offenders <- offenders[, c("lat", 'long')]
  

# Find all sex offenders within 1 mile of the house
result <- data.frame(house_index = numeric(), sex_offender_index = numeric(), distance = numeric())
for (i in 1:nrow(houses)) {
  distances <- sapply(1:nrow(sex_offenders), function(j) {
    haversine(houses$latitude[i], houses$longitude[i], sex_offenders$lat[j], sex_offenders$long[j])
  })
  within_1_mile <- which(distances <= 1)
  temp_result <- data.frame(
    house_index = rep(i, length(within_1_mile)),
    sex_offender_index = within_1_mile,
    distance = distances[within_1_mile]
  )
  result <- rbind(result, temp_result)
}

print(result)

nearest_offender <- data.frame(house_id = numeric(),
                               offender_id = numeric(),
                               distance = numeric())
for (i in 1:nrow(houses)) {
  
  # Calculate the distances between the current house and each offender
  distances <- distHaversine(offenders[,c("long","lat")], houses[i,c("longitude","latitude")])
  
  # Find the offender with the minimum distance
  nearest <- which.min(distances)
  nearest_offender <- rbind(nearest_offender, 
                            data.frame(house_id = houses[i, "house_index"],
                                       offender_id = offenders[nearest, "house_index"],
                                       distance = distances[nearest]))
}




offenders$distance <- distHaversine(offenders[,c("long","lat")], c(long,lat))

# Filter the data to only show sex offenders within 0.5 miles
offenders_nearby <- offenders %>%
  filter(distance <= 0.5)

# Get the number of sex offenders within 0.5 miles
num_offenders_nearby <- nrow(offenders_nearby)

# Print the number of sex offenders within 0.5 miles
print(paste("Number of sex offenders within 0.5 miles:", num_offenders_nearby))

nearest_offender

# view (houses_data[2653 ,])
# offenders[82,4]
# 
#  offenders[27,4]
# view(houses_data[6980,])
# 
# offenders[82,4]
# view( houses_data[9770 ,])
# 
# # offenders[ 82,4]
# view(houses_data[12250    ,])
# 
# # offenders[27,4]
# view(houses_data[12374   ,])
# 
# offenders[16 ,4]
# houses_data[16786 ,1]
