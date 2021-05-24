## add corresponding age_group_id from causes ages
f <- function(max_age){
      age_group_id = seq(25, 95, 5)[
        findInterval(
          max_age,
          seq(25, 90, 5),
          left.open =  TRUE
        ) #+ 1
      ]
      return(age_group_id)
}

f(1)

cbind(1:100, sapply(1:100, f))
