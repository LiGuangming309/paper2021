#openaq
install.packages("ropenaq")
library("ropenaq")

delhi_locations <- aq_locations(
  city = "Delhi", 
  country = "IN", 
  parameter = "pm25"
)

countries <- aq_countries()
aq_latest(country = "IN")

data <- aq_measurements(location = "London+Westminster",
                        parameter = "pm25")
