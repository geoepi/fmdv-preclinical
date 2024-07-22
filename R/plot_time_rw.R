plot_time_rw <- function(df, xlabel = "Days Post Exposure (DPE)", ylabel = "Deviation from Mean") {
  
  p <- ggplot(data = df, aes(x = DPE, y = Q_0.5)) +
    geom_hline(yintercept = 0,
               linetype = "longdash",
               linewidth = 1,
               col = "gray50") +
    geom_ribbon(aes(ymin=Q_0.025, ymax=Q_0.975), fill="steelblue", alpha=0.5) +
    geom_ribbon(aes(ymin=Q_0.25, ymax=Q_0.75), fill="steelblue", alpha=0.5) +
    geom_line(col="black", linewidth = 1) + 
    scale_color_manual(values = c('gray50', 'darkorange')) +
    labs(title = " ",
         color = "Significant",
         x = xlabel,
         y = ylabel) +
    scale_x_continuous(breaks= seq(0, 8, 2), labels = seq(0, 8, 2), limits = c(0, 8)) +
    scale_y_continuous(breaks= seq(-6, 4, 2), labels = seq(-6, 4, 2), limits = c(-6, 4)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1,0.75,1,0.75),"cm"),
          legend.direction = "horizontal",
          legend.position="none",
          strip.text = element_blank(), 
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=18, face="bold"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.title.y = element_text(size=22, face="bold"),
          axis.text.x = element_text(face="bold", size=18, vjust=0.5, 
                                     hjust=0.5, angle=0),
          axis.text.y = element_text(size=18, face="bold"),
          plot.title = element_text(size=22, face="bold"))
  
  return(p)
}