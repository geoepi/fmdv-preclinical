plot_animals_linerange <- function(df, xlabel = "Animal", ylabel = "Deviation from Mean") {
  
  df <- df %>%
    mutate(sig = if_else(Q_0.05 < 0 & Q_0.95 < 0, 1,
                        if_else(Q_0.05 > 0 & Q_0.95 > 0, 1, 0)))
  
  df$sig <- as.factor(df$sig)
  
  p <- ggplot(data = df, aes(x = Animal, y = Q_0.5, color = sig)) +
    geom_hline(yintercept = 0,
               linetype = "longdash",
               linewidth = 1,
               col = "gray50") +
    geom_linerange(aes(ymin = Q_0.025, ymax = Q_0.975), size = 2.5, alpha=0.3) +
    geom_linerange(aes(ymin = Q_0.25, ymax = Q_0.75), size = 3.5, alpha=0.5) +
    geom_point(aes(col=sig), size = 6) + 
    scale_color_manual(values = c('gray50', 'darkorange')) +
    labs(title = " ",
         color = "Significant",
         x = xlabel,
         y = ylabel) +
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
          axis.text.x = element_text(face="bold", size=15, vjust=0.5, 
                                     hjust=0.5, angle=45),
          axis.text.y = element_text(size=18, face="bold"),
          plot.title = element_text(size=22, face="bold"))
  
  return(p)
}