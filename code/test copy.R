a<- data.frame(A = c(NA,1:3),B=c(5:8))
b<- data.frame(A = c(NA,1:2), C = 8:10)

full_join(a,b,"A")
