plot_median_phases <- function(samples_inc, samples_lat, x_max = 5, xlabel = "Duration (Days)", 
                                   ylabel = "Probability") {
  
  p <- ggplot(data = medians_df, aes(x = Name, y = quant0.5, color = Name)) +
    geom_linerange(aes(ymin = quant0.025, ymax = quant0.975), linewidth = 3.5, alpha=0.3) +
    geom_linerange(aes(ymin = quant0.25, ymax = quant0.75), linewidth = 4.5, alpha=0.5) +
    geom_point(size = 8, aes(color = Name)) + 
    scale_y_continuous(breaks = seq(0, 7, by = 1), limits = c(0, 7)) +
    scale_color_manual(values = c('steelblue', 'darkorange', "darkgreen")) +
    labs(title = " ",
         color = "Phase",
         x = "",
         y = "Median Duration (Days)") +
    theme_minimal() +
    coord_flip() +
    theme(plot.margin = unit(c(1,0.75,1,0.75),"cm"),
          legend.direction = "horizontal",
          legend.position="bottom",
          strip.text = element_blank(), 
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=18, face="bold"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.title.y = element_text(size=22, face="bold"),
          axis.text.x = element_text(face="bold", size=15, vjust=0.5, 
                                     hjust=0.5, angle=0),
          axis.text.y = element_text(size=18, face="bold"),
          plot.title = element_text(size=22, face="bold"))
  
  return(p)
}