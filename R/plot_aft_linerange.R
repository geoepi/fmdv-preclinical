plot_aft_linerange <- function(aft_model, xlabel = " ", ylabel = "Duration (days)", y_min = 1, y_max = 8, brks = 2, ylimit = 8) {
  
  df <- aft_model$summary.random$group[,c(1,4,6,7,8, 10)]
  names(df) <- c("Group","Q_0.025","Q_0.25", "Q_0.5", "Q_0.75", "Q_0.975")
  mean_aft <- aft_model$summary.fixed$mean
  
  df <- df %>%
    mutate(sig = if_else(Q_0.025 < 0 & Q_0.975 < 0, 1,
                         if_else(Q_0.025 > 0 & Q_0.975 > 0, 1, 0)))
  
  df$sig <- as.factor(df$sig)
  
  df[,2:6] <- exp(df[,2:6] + mean_aft)
  
  p <- ggplot(data = df, aes(x = Group, y = Q_0.5, color = sig)) +
    geom_hline(yintercept = exp(mean_aft),
                linetype = "longdash",
                linewidth = 1,
                col = "gray50") + 
    annotate("text", x = Inf, y = exp(mean_aft) - 0.2, 
             label = sprintf("(mean = %.2f)", exp(mean_aft)), 
             hjust = 9.9, vjust = 0, size = 4, col="gray40") +
    geom_linerange(aes(ymin = Q_0.025, ymax = Q_0.975), size = 2.5, alpha=0.3) +
    geom_linerange(aes(ymin = Q_0.25, ymax = Q_0.75), size = 3.5, alpha=0.5) +
    geom_point(aes(col=sig), size = 6) + 
    scale_color_manual(values = c('gray50', 'darkorange')) +
    labs(title = " ",
         color = "Significant",
         x = xlabel,
         y = ylabel) +
    scale_y_continuous(breaks= seq(y_min, y_max, brks), labels = seq(y_min, y_max, brks), limits = c(0, ylimit)) +
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