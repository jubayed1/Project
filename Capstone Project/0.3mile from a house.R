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

result <- data.frame(house = integer(), offender = integer(), distance = numeric(), photo_date = as.Date(character()), off_market_date = as.Date(character()))
for (i in 1:nrow(houses)) {
  for (j in 1:nrow(offenders)) {
    distance_in_km <- haversine_distance(houses[i, "latitude"], houses[i, "longitude"],
                                         offenders[j, "lat"], offenders[j, "long"])
    distance_in_miles <- distance_in_km * 0.621371
    
    if (distance_in_miles <= 0.3) {
      photo_date <- offenders[j, "Photo.Date"]
      off_market_date <- houses[i, "off_market_date"]
      result <- rbind(result, data.frame(house = i, offender = j, distance = distance_in_miles, photo_date = photo_date, off_market_date = off_market_date))
    }
  }
}

result$before_sale <- result$Photo.Date < result$off_market_date
filtered_result <- result[result$before_sale, ]
###5864 house have offenders within 0.3 miles
### in 552 houses offender lived before it sold

n_offenders_near_house <- numeric(nrow(houses))

for (i in 1:nrow(houses)) {
  house_lat <- houses$latitude[i]
  house_lon <- houses$longitude[i]
  
  # Calculate the distance between the house and each offender
  offenders$distance <- distHaversine(cbind(offenders$long, offenders$lat),
                                      cbind(house_lon, house_lat))
  
  # Filter the database to only include offenders within 0.3 miles of the house
  offenders_near_house <- offenders %>%
    filter(distance <= 482.8) # 0.3 mile = 482.8 meters
  
  # Count the number of offenders within 0.3 miles of the house
  n_offenders_near_house[i] <- nrow(offenders_near_house)
}

# Add the results to the data frame for the houses
houses$n_offenders_near_house <- n_offenders_near_house

# Filter houses with at least one offender within 0.3 miles (optional)
# houses_with_offender <- houses %>%
#  filter(n_offenders_near_house >= 1)

# Output all houses with the number of offenders within 0.3 miles
houses
final0.3<- houses%>% 
  filter(soldprice!="NA")
final_data$offenders_0.3<- final0.3$n_offenders_near_house
reg0.3<- lm(ln_soldprice~offenders_0.3, data = final_data)
summary(reg0.3)

# Add the fixed effects dummy variables to the regression equation
regprice4 <- lm(ln_soldprice ~ offenders_near_house + age_05_or_less + age_06_10 + age_11_15 + age_16_20 + age_21_25 + age_26_30 + age_31_35 + age_36_40 + age_41_50 + age_51_60 + age_61_70 + age_71_80 + age_81 +
                  age_unknown + beds + fb + hb + sqft_1001_1200 + sqft_1201_1400 + sqft_1401_1600 + sqft_1601_1800 + sqft_1801_2000 +
                  sqft_2001_2200 + sqft_2201_2500 + sqft_2501_2750 + sqft_2751_3000 + sqft_3001_3300 + sqft_3301_3750 +
                  sqft_3751_4250 + sqft_4251_5000 + sqft_5001_5500 + lotsize_use + tract_dummies + year_dummies,
                data = final_data)

# View the regression results
summary(regprice4)
