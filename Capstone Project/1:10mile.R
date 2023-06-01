

offenders<- read_excel('geocoded_offender.xlsx')
houses <- read_excel("warren.xlsx")


final<- read_excel("final.xlsx")
ln_soldprice<- log(final$soldprice)
regtenth<- lm(ln_soldprice~n_offenders_near_house+year__2012+year__2013+year__2014+year__2015+year__2016+year__2017+year__2018+year__2019+
                year__2020, data = final)
summary(regtenth)
age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+
  age_unknown+beds+fb+hb+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
  sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
  sqft_3751_4250+sqft_4251_5000+sqft_5001_5500+sqft_5501_6000+sqft_6000+ lotsize_use
reg_day<-lm(daysonmarket~n_offenders_near_house, data = final)
summary(reg_day)
coeftest(reg_day, vcov. = vcovHC(reg_day, type = "HC2"))








#within 1 mile of houses to offenders and the distance
houses$off_market_date <- as.Date(houses$off_market_date, format = "%m/%d/%Y")
offenders$Photo.Date <- as.Date(offenders$Photo.Date, format = "%m/%d/%Y")
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
    
    if (distance_in_miles <= 0.1) {
      photo_date <- offenders[j, "Photo.Date"]
      off_market_date <- houses[i, "off_market_date"]
      result <- rbind(result, data.frame(house = i, offender = j, distance = distance_in_miles, photo_date = photo_date, off_market_date = off_market_date))
    }
  }
}


result$before_sale <- result$Photo.Date < result$off_market_date
filtered_result <- result[result$before_sale, ]


# Merge the filtered result data with the houses data by matching on the row names
merged <- merge(houses, filtered_result, by.x = "row.names", by.y = "house", all = TRUE) %>%
  group_by(id) %>%
  mutate(rowforhouse=row_number()) %>%
  ungroup() %>%
  filter(rowforhouse==1)

merged3 <- merge(merged,houses, by = "id")
merged3<- merged3[,!names(merged3) %in% c("soldprice.x", "Photo.Date", "off_market_date.y", 'offender')]
merged3<-merged3%>%
  rename('soldprice' = 'soldprice.y')

final<- houses %>%
  select("askingprice":"lotsize_use",'soldprice','year_sold','month_sold','n_offenders_near_house','tract','daysonmarket' )%>%
  filter(soldprice!='NA')

final$soldprice <- gsub("[^0-9.]", "", final$soldprice) ##removing all na and non-number
final$soldprice <- na.omit(as.numeric(final$soldprice))

library(fastDummies)
dummy_vars <- dummy_cols(final$tract)
col_names <- colnames(dummy_vars)
new_col_names <- gsub("tract_",".data", col_names)
colnames(dummy_vars) <- new_col_names
final<- cbind(final_data, dummy_vars)
ln_soldprice<- log(final$soldprice)

final<-final%>% select('askingprice':'tract_212270119')
reg3<-lm(ln_soldprice~age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+
           age_unknown+beds+fb+hb+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
           sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
           sqft_3751_4250+sqft_4251_5000+sqft_5001_5500+sqft_5501_6000+sqft_6000+ lotsize_use+n_offenders_near_house+year_sold+month_sold, data = final)
summary(reg3)
####coeftest(reg1,vcov. = vcovHC(reg1, type='HC2'))
ggplot(final_data,aes(x=ln_soldprice,y=offenders_near_house)) +
  geom_point() 
###omitted variables(,sqft_1000,tract_212270101) +
##tract_212270106+tract_212270107+tract_212270108+tract_212270109+tract_212270110+tract_212270111+tract_212270112+tract_212270113+tract_212270114+tract_212270115+tract_212270116+tract_212270117+tract_212270118+tract_212270119, data = final_data)


reg4<- lm(daysonmarket~n_offenders_near_house+year_sold+month_sold+age_05_or_less+age_06_10+age_11_15+age_16_20+age_21_25+age_26_30+age_31_35+age_36_40+age_41_50+age_51_60+ age_61_70+age_71_80+age_81+
            age_unknown+beds+fb+hb+sqft_1001_1200+sqft_1201_1400+sqft_1401_1600+sqft_1601_1800+sqft_1801_2000+
            sqft_2001_2200+sqft_2201_2500+sqft_2501_2750+sqft_2751_3000+sqft_3001_3300+sqft_3301_3750+
            sqft_3751_4250+sqft_4251_5000+sqft_5001_5500+sqft_5501_6000+sqft_6000+ lotsize_use, data = final)
stargazer(reg3,reg4, type = 'text')
n_offenders_near_house <- numeric(nrow(houses))

for (i in 1:nrow(houses)) {
  house_lat <- houses$latitude[i]
  house_lon <- houses$longitude[i]
  
  # Calculate the distance between the house and each offender
  offenders$distance <- distHaversine(cbind(offenders$long, offenders$lat),
                                      cbind(house_lon, house_lat))
  
  # Filter the database to only include offenders within 0.1 mile of the house
  offenders_near_house <- offenders %>%
    filter(distance <= 160.93) # 0.1 mile = 160.93 meters
  
  # Count the number of offenders within 0.1 mile of the house
  n_offenders_near_house[i] <- nrow(offenders_near_house)
}

# Add the results to the data frame for the houses
houses$n_offenders_near_house_O.1 <- n_offenders_near_house
# houses_with_offender<- houses %>%
#   filter(n_offenders_near_house >= 1 & n_offenders_near_house <= 3)
# rename()
# Print the results
print(houses_with_offender)
write_xlsx(final, 'final.xlsx')
 plot(final$soldprice, final$n_offenders_near_house)
 