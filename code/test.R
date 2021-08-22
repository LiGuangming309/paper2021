a <- setdiff(colnames(attrBurden), c("Race", "Hispanic.Origin", "lower", "mean", "upper"))
b<- c("Race.x", "Hispanic.Origin.x", "Race.y", "Hispanic.Origin.y", "Year", "Gender.Code", "Education", "Region", "measure1", "measure2", "source", "agr_by")

setdiff(a,b)
