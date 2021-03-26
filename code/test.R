#install.packages('gmodels')
library(gmodels)
library(dplyr)
library(magrittr)
library(Rmisc)
library(ggplot2)

cvd_ihd_25 <- read.csv("~/Desktop/cvd_ihd_25.csv")
columns<- paste0("draw_",0:999)

df1 <- cvd_ihd_25[,c("exposure_spline","mean","lower","upper")]
df1$source <- "im paper"

df2<-apply(cvd_ihd_25[,columns], 1, function(row){ Rmisc::CI(row, ci=0.95)} )
df2 <- t(df2) %>% as.data.frame
df2 <- data.frame(exposure_spline=cvd_ihd_25$exposure_spline,
                  mean = df2[, "mean"],
                  lower = df2$lower,
                  upper = df2$upper)
df2$source <- "Rmisc"

#classes <- c("numeric", 'lm', 'lme', 'estimable', 'fit_contrast')
classes <- c("numeric")
df3 <- lapply(classes, function(class){
  df3 <- apply(cvd_ihd_25[,columns], 1, function(row){ 
    class(row) <- class
    gmodels::ci(row,confidence=0.95)} )
  df3 <- t(df3) %>% as.data.frame
  df3 <- data.frame(exposure_spline=cvd_ihd_25$exposure_spline,
                    mean = df3[, "Estimate"], 
                    lower = df3[, "CI lower"],
                    upper = df3[, "CI upper"])
  
  df3$source <- paste0("gmodels, ", class)
  return(df3)
}) %>% do.call(rbind,.)

df4 <- rbind(df1, df2, df3)
df4 <- df4 %>% filter(exposure_spline <= 50)

g<-ggplot(data=df4, 
          aes(x=exposure_spline, y=mean, color = source)) + 
  geom_point() + 
  geom_line()

g<-g+geom_ribbon(aes(ymin=lower, ymax=upper, color = source), linetype=2, alpha=0.1)
g

