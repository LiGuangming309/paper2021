burnett_form <- function(X, theta, alpha, mu, v) {
  X <- pmax(0, X - 2.4)
  one <- log(1 + (X / alpha))
  two <- 1 / (1 + exp(-(X - mu) / v))
  Y <- exp(theta * one * two)
  return(Y)
}
pm <- seq(0,84, by = 0.1)

thetas1 <- rnorm(1000, 0.1430, 0.01807)
hr_epa1 <- outer(pm,
              thetas1,
              function(pm, theta){
                burnett_form(pm, theta, 1.6, 15.5, 36.8)
              })

hr_epa1 <- data.frame(pm = pm,
                 lower = matrixStats::rowQuantiles(hr_epa1, probs = 0.25),
                 mean = rowMeans(hr_epa1),
                 upper = matrixStats::rowQuantiles(hr_epa1, probs = 0.75)
)

ggplot(hr_epa1, aes(x = pm)) +
  geom_point(aes(y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.33)

thetas2 <- c(0.1430- 2*0.01807, 0.1430, 0.1430+ 2*0.01807)
hr_epa2 <- outer(pm,
              thetas2,
              function(pm, theta){
                burnett_form(pm, theta, 1.6, 15.5, 36.8)
              }) 

hr_epa2 <- data.frame(pm = pm,
                      lower = hr_epa2[,1],
                      mean = hr_epa2[,2],
                      upper = hr_epa2[,3]
)

g2 <- ggplot(hr_epa2, aes(x = pm)) +
  geom_point(aes(y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.33) +
  theme_classic() 

g2 <- g2 +theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA)
)
#returns white background
ggsave("airquality.png", g2, bg = "transparent")
png('tr_tst2.png',width=300,height=300,units="px",bg = "transparent")
print(g2)
dev.off()
