max_age <- 1:80
age_group_id = c(0,seq(25, 95, 5))[
  findInterval(
    max_age,
    c(0,seq(25, 95, 5)),
    left.open =  F
  ) 
]

cbind(max_age, age_group_id)
