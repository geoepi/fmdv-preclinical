plot_incidence_bar <- function(df, 
                               ylimits = c(0, 22000),
                               legend_title = "Compartment",
                               plot_title = " ", vline = NULL,
                               xlab = " ", ylab = " ") {
  
  df <- df %>%
    filter(compartment %in% c("clin_inc", "sub_inc")) 
  
  df[,3:7] <- round(df[,3:7], 0)
  
  p <- ggplot(df, aes(x = time, y = Q_0.5, fill = compartment)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = " ",
      x = "Time (Days)",
      y = "Incidence"
    ) +
    scale_fill_manual(values = c("#1F78B4", "#B15928"),
                      name = " ") +
    scale_color_manual(values = c("#1F78B4", "#B15928")) + 
    theme_minimal() +
    theme(plot.margin = unit(c(2,0.5,2,0.5),"cm"),
          legend.direction = "horizontal",
          legend.position="bottom", 
          strip.text = element_blank(), #element_text(size=18, face="bold"),
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=18, face="bold"),
          axis.title.x = element_text(size=24, face="bold"),
          axis.title.y = element_text(size=24, face="bold"),
          axis.text.x = element_text(face="bold", size=20, vjust=1, 
                                     hjust=0.5, angle=0),
          axis.text.y = element_text(size=20, face="bold"),
          plot.title = element_text(size=22, face="bold"))
  
  return(p)
  
  
}
