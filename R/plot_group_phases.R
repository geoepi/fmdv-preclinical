plot_group_phases <- function(df, samples_lat, xlabel = "Duration (Days)", 
                                   ylabel = " ", ymax=7.5) {
  
  df$Group[df$Group == "study"] <- "Study-wide"
  
  df$Phase <- factor(df$Phase, levels = c("Latent", "Subclinical", "Incubation"))
  
  p <- ggplot(df, aes(x = Group, color = Phase)) +
      geom_point(aes(y = Q_0.5), position = position_dodge(width = 0.5), size = 6) +
      geom_linerange(aes(ymin = Q_0.025, ymax = Q_0.975),
                     position = position_dodge(width = 0.5), linewidth = 3.5, alpha = 0.5) +
      geom_linerange(aes(ymin = Q_0.25, ymax = Q_0.75),
                     position = position_dodge(width = 0.5), linewidth = 4.5, alpha = 0.7) +
      scale_x_discrete(expand = c(0.5, 0.2)) +
      scale_y_continuous(breaks = seq(0, ymax, by = 1), limits = c(0, ymax)) +
      scale_color_manual(values = c('steelblue', 'darkorange', "darkgreen")) +
      theme_minimal() +
      labs(title = " ",
           x = ylabel,
           y = xlabel) +
    theme_minimal() +
    #coord_flip() +
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
