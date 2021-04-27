

attr_burd_2001 <- read.csv("~/Desktop/paper2021/data/13_attr_burd/nation/wonder/attr_burd_2001.csv")
attr_burd_alt_2001 <- read.csv("~/Desktop/paper2021/data/13_attr_burd/nation/nvss/attr_burd_alt_2001.csv")

setdiff(colnames(attr_burd_alt_2001), colnames(attr_burd_2001))
colnames(attr_burd_2001)
colnames(attr_burd_alt_2001)
rbind(attr_burd_2001, attr_burd_alt_2001)
