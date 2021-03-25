#install.packages('gmodels')
library(gmodels)
library(dplyr)
library(magrittr)
library(Rmisc)

cvd_ihd_25 <- read.csv("~/Desktop/cvd_ihd_25.csv")
columns<- paste0("draw_",0:999)

df<-apply(cvd_ihd_25[,columns], 1, function(row){
  a1 <- CI(row, ci=0.95)
  a2 <- ci(row)
  #t[1:3]
  return(c(a1,a2))
} )
df <- t(df)
df <- cbind(cvd_ihd_25[,c("exposure_spline","mean","lower","upper")],
            df)


