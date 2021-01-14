replaces3 <- data.frame(
  from = states$STATEFP,
  to = 1:49
)

pafs<-as.data.frame(pafs)
replaces3<-as.data.frame(replaces3)
pafs <- DataCombine::FindReplace(data = pafs, Var = "STATEFP", replaceData = replaces3, from = "from", to = "to", exact = FALSE)

replaces2 <- data.frame(
  from = c("WHITE", "AMERICAN INDIAN AND ALASKA NATIVE", "ASIAN OR PACIFIC ISLANDER", "BLACK OR AFRICAN AMERICAN"),
  to = c("White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Black or African American")
)
pafs <- DataCombine::FindReplace(data = pafs, Var = "STATEFP", replaceData = replaces2, from = "from", to = "to", exact = FALSE)
