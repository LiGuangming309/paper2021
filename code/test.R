

legend_colour <- matrix(NA, ncol = ncol(legend)-1, nrow= nrow(legend))
legend_colour[,1] <- legend$colour 
legend_label <- legend[,-1]  %>% as.matrix()
legend_order <- matrix(1:prod(dim(legend_label)), ncol=ncol(legend_label),byrow = FALSE)

legend_label <- matrix("", ncol = ncol(legend)-1, nrow= nrow(legend))
c(legend_label)[legend_order]
c(legend_colour)[legend_order]

plot.new()
legend("bottom",
       c(legend_label)[legend_order],
       lty="solid",
       bty="n", 
       col=c(legend_colour)[legend_order],
       ncol=ncol(legend_label),
       cex=0.5, pch=1, pt.cex = 1,
       #y.intersp=0.5,
       x.intersp=10,text.width=0.1
       )

