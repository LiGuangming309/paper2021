findreplaces1 <-
  lapply(c("", 0:9), function(end) {
    data.frame(
      replacecolumns = "label_cause",
      from = c(
        "I20", "I21", "I22", "I23", "I24", "I25",
        "G45", "G46", "I61", "I62", "I63", "I65", "I66", "I67", "I68", "I69",
        "C33", "C34", "D02", "D14", "D38",
        "J41", "J42", "J43", "J44",
        "A48", "A70", "B97", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J20", "J21", "P23", "U04",
        "E11",
        sprintf("C%02d", 0:97), sprintf("D%02d", 0:48), sprintf("D%02d", 55:89),
        sprintf("E%02d", 3:7), sprintf("E%02d", 10:16), sprintf("E%02d", 20:34),
        sprintf("E%02d", 65:88), sprintf("F%02d", 1:99),sprintf("F%02d", 1:99),
        sprintf("G%02d", 6:98), sprintf("H%02d", 0:61), sprintf("H%02d", 68:93),
        sprintf("I%02d", 0:99), sprintf("J%02d", 30:98), sprintf("K%02d", 0:92),
        sprintf("N%02d", 0:64), sprintf("N%02d", 75:98), sprintf("L%02d", 0:98),
        sprintf("M%02d", 0:99), sprintf("Q%02d", 0:99),
        #lower respitory infections
        sprintf("J%02d", 9:22)
      ),
      to = c(rep("cvd_ihd", 6), rep("cvd_stroke", 10), rep("neo_lung", 5), rep("resp_copd", 4), rep("lri", 15), "t2_dm",
             rep("ncd_lri", 1276))
      
    ) %>% mutate(from = paste0(from, end))
  }) %>% do.call(rbind, .)

#non-communicable diseases
#https://www150.statcan.gc.ca/n1/pub/82-003-x/2013007/article/11852/tbl/appb-eng.htm
ncd_lri <- c(sprintf("C%02d", 0:97), sprintf("D%02d", 0:48), sprintf("D%02d", 55:89),
         sprintf("E%02d", 3:7), sprintf("E%02d", 10:16), sprintf("E%02d", 20:34),
         sprintf("E%02d", 65:88), sprintf("F%02d", 1:99),sprintf("F%02d", 1:99),
         sprintf("G%02d", 6:98), sprintf("H%02d", 0:61), sprintf("H%02d", 68:93),
         sprintf("I%02d", 0:99), sprintf("J%02d", 30:98), sprintf("K%02d", 0:92),
         sprintf("N%02d", 0:64), sprintf("N%02d", 75:98), sprintf("L%02d", 0:98),
         sprintf("M%02d", 0:99), sprintf("Q%02d", 0:99),
         #lower respitory infections
         sprintf("J%02d", 9:22)
         )



xx <- sprintf("%1$d %1$x %1$X", 0:15)
xx <- matrix(xx, dimnames = list(rep("", 16), "%d%x%X"))
noquote(format(xx, justify = "right"))
