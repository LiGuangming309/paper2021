#install.packages('gmodels')
library(gmodels)
library(dplyr)
library(magrittr)
library(Rmisc)
library(ggplot2)
library(data.table)

a <- colnames(total_burden)
b <- colnames(total_burden_yll)

setdiff(b,a)
