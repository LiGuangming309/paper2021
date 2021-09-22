library(gridExtra)
library(ggplot2)

dat <- data.frame(
  label = c("A", "B", "C"),
  point_est = c(1,2,3),
  lb_ci = c(.5, 1.5, 2.5),
  ub_ci = c(1.5, 2.5, 3.5),
  n = c(50, 100, 150),
  total = c(75, 150, 200)
)

plot1 <- ggplot(dat, aes(x=point_est, y=label)) +
  geom_point() +
  geom_errorbarh(aes(xmin=lb_ci, xmax=ub_ci), height=.5) +
  ggtitle("Some measure") +
  ylab(NULL) + xlab("some effect estimate")

tab_base <- ggplot(dat, aes(y=label)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), ## centering title on text
        axis.text.x=element_text(color="white"), ## need text to be printed so it stays aligned with figure but white so it's invisible
        axis.line=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

tab1 <- tab_base + 
  geom_text(aes(x=1, label=n)) + 
  ggtitle("n")

tab2 <- tab_base +
  geom_text(aes(x=1, label=total)) + 
  ggtitle("total")

lay <-  matrix(c(1,1,1,1,1,1,2,3), nrow=1)
grid.arrange(plot1, tab1, tab2, layout_matrix = lay)
