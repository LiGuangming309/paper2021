#openaq
install.packages("ropenaq")
# install.packages("remotes")
remove.packages("ropenaq")
remotes::install_github("ropensci/ropenaq")
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

library("ropenaq")
countries_table <- aq_countries()
