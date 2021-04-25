library(devtools)
install_github("FridljDa/paper2021/data/14_summary")
test <- read.csv("https://github.com/FridljDa/paper2021/blob/master/data/14_summary/all_burd.csv")
#"https://raw.github.com/FridljDa/paper2021/master/master"
test <- read.csv("https://raw.github.com/FridljDa/paper2021/blob/master/master")
#https://github.com/FridljDa/paper2021/blob/45829c4a46e251e91f6d0a069581b84472300c1d/data/14_summary/all_burd.csv
test <- read.csv("https://github.com/FridljDa/paper2021/blob/45829c4a46e251e91f6d0a069581b84472300c1d/data/14_summary/all_burd.csv")

library(RCurl)
x <- getURL("https://github.com/FridljDa/paper2021/blob/master/data/14_summary/all_burd.csv")
y <- read.csv(text = x)
#latent.growth.data <- read.csv("https://github.com/FridljDa/paper2021/blob/master/data/14_summary/all_burd.csv")

y <-download.file("https://raw.github.com/FridljDa/paper2021/blob/master/data/14_summary/all_burd.csv", 
              destfile = "/tmp/test.csv", method = "curl")

library(RCurl)
x <- getURL("https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv")
y <- read.csv(text = x)

download.file("https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv", 
              destfile = "/tmp/test.csv", method = "curl")

urlfile<-'https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv'
urlfile<-'https://raw.github.com/FridljDa/paper2021/master/data/14_summary/all_burd.csv'
dsin<-read.csv(urlfile)
