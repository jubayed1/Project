dummy_vars <- dummy_cols(prime_data$offender_lived_1)
col_names <- colnames(dummy_vars)
new_col_names <- gsub(".data","offender", col_names)
colnames(dummy_vars) <- new_col_names
final_data <- cbind(final_data, dummy_vars)
prime_data<- cbind(prime_data,offender_presence)
prime_data<- prime_data%>%
  mutate(offender_presence=ifelse(offender_lived_1>0,1,0))
reg_presene<-lm(ln_soldprice~offender_presence, data = prime_data)
summary(reg_presene)
graph_data<- final_data%>%
  select('soldprice','distance')%>%
 na.omit()
graph_data$distance<-as.numeric(graph_data$distance)
reg_dis<-lm(soldprice~distance, data = graph_data)
summary(reg_dis)
ggplot(graph_data, aes(x = distance, y = soldprice)) +
  geom_point() +
  geom_line(aes(y = reg_dis$fitted.values, x = distance), color = "red") +
  labs(y = "Sold Price", x = "Minimum Distance to an Offender") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma_format()) +
  guides(color = FALSE)
regprice1 <- na.omit(regprice1)
regprice1_fix_house <- na.omit(regprice1_fix_house)
regprice1_house_time <- na.omit(regprice1_house_time)
regprice1_location <- na.omit(regprice1_location)
stargazer(regprice1, regprice1_fix_house, regprice1_house_time, regprice1_location, type = "text", out = "models.txt")
