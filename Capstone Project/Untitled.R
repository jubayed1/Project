library(dplyr)
library(leaflet)

house_lat <-36.99796

house_long <- -86.45105
d
library(geosphere)

# Define latitude and longitude of the house and the offenders
house <- data.frame(lat = 36.99796, long = -86.45105)
offenders <- offender[, c("lat", 'long')]

# Calculate distance between the house and each offender
dist <- distm(offenders[, c("long", "lat")], house[, c("long", "lat")], fun = distHaversine)

# Convert distance to miles and create logical vector indicating which offenders are within 0.5 miles radius from the house
dist_miles <- dist/1609.34 # convert distance to miles
within_radius <- dist_miles <= 0.1 # create logical vector

# Subset the offenders data frame to get the information about the offenders within 0.5 miles radius from the house
offenders_within_radius <- offenders[within_radius, ]

# Create a vector of your variable values
variable <- d[,'Description']
# Extract the Count values from the variable
counts <- sapply(strsplit(variable, "\\|"), function(x) as.numeric(x[2]))

# Extract the Description values from the variable
descriptions <- sapply(strsplit(variable, "\\|"), function(x) x[4])

# Aggregate the counts by description
counts_by_description <- tapply(counts, descriptions, sum)

# Print the counts by description in descending order
sort(counts_by_description, decreasing = TRUE)
################
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 # Earth's radius in kilometers
  dLat <- (lat2 - lat1) * pi/180
  dLon <- (lon2 - lon1) * pi/180
  lat1 <- lat1 * pi/180
  lat2 <- lat2 * pi/180
  
  a <- sin(dLat/2)^2 + cos(lat1) * cos(lat2) * sin(dLon/2)^2
  c <- 2 * asin(sqrt(a))
  d <- R * c
  return(d)
}

houses$min_distance <- NA

for (i in 1:nrow(houses)) {
  min_distance <- Inf
  for (j in 1:nrow(offenders)) {
    distance_in_km <- haversine_distance(houses[i, "latitude"], houses[i, "longitude"],
                                         offenders[j, "lat"], offenders[j, "long"])
    distance_in_miles <- distance_in_km * 0.621371
    
    if (distance_in_miles <= 1) {
      if (distance_in_miles < min_distance) {
        min_distance <- distance_in_miles
      }
    }
  }
  if (min_distance != Inf) {
    houses$min_distance[i] <- min_distance
  }
}

library(dplyr)
library(knitr)

# Compute summary statistics for all variables in the prime data frame
summary_stats <- prime_data %>% 
  select("soldprice","offenders_lived_0.1","offender_lived_1","lotsize_use","beds",'fb','hb','tract_',"appearance_fair","AC_electric","attached_garage","units","month_sold", "sold_year")
  
library(ggplot2)
# Format the summary statistics as a table using the kable() function from the knitr package
kable(summary_stats, format = "markdown")
summary(summary_stats)
ggplot(final_data, aes(x=soldprice,y=distance))+geom_point()+labs(x="Sold Price", y="Minimun Distance to an offender")
############New regressio



stargazer(reg_presence, type = "text")
