
# Single.Year.Ages.Code
dataDir <- "/Users/default/Desktop/paper2021/data"
lifeExpectancy <- read.csv(file.path(dataDir, "IHME_GBD_2019_TMRLT_Y2021M01D05.csv"))

x <- c(0:10,20,90,91,92,93,94,95,96)

v<-lifeExpectancy$Life.Expectancy[findInterval(x, lifeExpectancy$Age)]


cbind(x,v)
v

