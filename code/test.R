your_df <- total_burden
ncd_lri <- c( # non-communicable diseases
  sprintf("C%02d", 0:97), sprintf("D%02d", 0:48), sprintf("D%02d", 55:89),
  sprintf("E%02d", 3:7), sprintf("E%02d", 10:16), sprintf("E%02d", 20:34),
  sprintf("E%02d", 65:88), sprintf("F%02d", 1:99), sprintf("F%02d", 1:99),
  sprintf("G%02d", 6:98), sprintf("H%02d", 0:61), sprintf("H%02d", 68:93),
  sprintf("I%02d", 0:99), sprintf("J%02d", 30:98), sprintf("K%02d", 0:92),
  sprintf("N%02d", 0:64), sprintf("N%02d", 75:98), sprintf("L%02d", 0:98),
  sprintf("M%02d", 0:99), sprintf("Q%02d", 0:99),
  # lower respitory infections
  sprintf("J%02d", 9:22)
)

tic("method 1")
ncd_lri1 <- c(outer(ncd_lri, c("", 0:9), FUN = paste0))
result1 <- filter(your_df, label_cause %in% ncd_lri1)
toc()

tic("method 2")
ncd_lri2<-paste(ncd_lri[1:2],collapse="|")
result2 <- filter(your_df, grepl(ncd_lri2, label_cause))
toc()

tic("method 3")
result3 <- filter(your_df, substring(label_cause, 1, 3) %in% ncd_lri)
toc()
