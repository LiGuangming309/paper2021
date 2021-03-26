#install.packages('gmodels')
library(gmodels)
library(dplyr)
library(magrittr)
library(Rmisc)
library(ggplot2)
library(data.table)

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

lower <- function(x){quantile(x,p=.025)}
upper <- function(x){quantile(x,p=.975)}
df4<-apply(cvd_ihd_25[,columns], 1, function(row){ 
  c(mean = mean(row),
    lower = lower(row),
    upper = upper(row))} )
df4 <- t(df4) %>% as.data.frame
df4 <- data.frame(exposure_spline=cvd_ihd_25$exposure_spline,
                  mean = df4[, 1],
                  lower = df4[, 2],
                  upper = df4[, 3])
df4$source <- "quantile"

df5 <- rbind(df1, df4)
df5 <- df5 %>% filter(exposure_spline <= 50)

g<-ggplot(data=df5, 
          aes(x=exposure_spline, y=mean, color = source)) + 
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper, color = source), linetype=2, alpha=0.1)

g

