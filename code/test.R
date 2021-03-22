#install.packages('gmodels')
library(gmodels)
library(magrittr)

cvd_ihd_25 <- read.csv("~/Downloads/cvd_ihd_25.csv")
columns<- paste0("draw_",0:999)
df<-data.frame(exposure_spline=cvd_ihd_25[,1], Mean_own=rowMeans(cvd_ihd_25[,columns]))
df <- cbind(df,
            cvd_ihd_25$mean)

x<-cvd_ihd_25[3,columns] %>% unlist
ci(x, confidence=0.95, alpha=1 - confidence)
