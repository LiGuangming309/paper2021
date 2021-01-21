
g <- attrBurden_gr_his %>%
  ggplot(aes_string(x = "Year", y = "Deaths", group = "Race", color = "Race")) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle(paste("hispanic origin:", his_or)) +
  theme_ipsum() +
  ylab(paste("burden measured in", measure)) +
  xlab("Year") +
  ylim(0, NA) +
  xlim(2000, 2016) +
  geom_line()

ggsave(file.path(plotsDir, paste0(measure, "_", his_or, ".png")),
       plot = g
)
