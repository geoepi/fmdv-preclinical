plot_fixed_posterior <- function(df, model = shed_model, xlabel = "Temperature", ylabel = "Coefficient", fixed_eff = "temp") {
  
  ci_intervals <- model$summary.fixed
  ci_intervals$name <- rownames(ci_intervals)
  
  temp_ci <- ci_intervals[fixed_eff,]
  
  p <- ggplot(as.data.frame(temp_post[[fixed_eff]]), aes(x, y)) +
    geom_line(col="black", linetype="solid") +
    geom_vline(xintercept = 0,
               linetype = "dotdash",
               linewidth = 1,
               col = "red") +
    geom_vline(xintercept = c(temp_ci[1,4], temp_ci[1,8]), 
               linetype = "longdash",
               linewidth = 1,
               col = "gray50") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +  
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
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
                                     hjust=0.5, angle=0),
          axis.text.y = element_text(size=18, face="bold"),
          plot.title = element_text(size=22, face="bold"))
  
  return(p)
  
}