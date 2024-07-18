plot_survival_marginals <- function(samples, x_max = 14, xlabel = "Incubation Phase Duration", 
                                    ylabel = "Probability") {

  p<- ggplot(samples, aes(time_vect, quant0.5)) +
    geom_ribbon(aes(ymin=quant0.025, ymax=quant0.975), fill="steelblue", alpha=0.5) +
    geom_ribbon(aes(ymin=quant0.25, ymax=quant0.75), fill="steelblue", alpha=0.5) +
    geom_line() +
    geom_hline(yintercept = 0.5,
               linetype = "longdash",
               linewidth = 1,
               col = "gray40") +
    xlab(xlabel) + 
    ylab(ylabel) +
    scale_x_continuous(breaks= seq(0, x_max, 1), labels = seq(0, x_max, 1), limits = c(0, x_max)) +
    theme_minimal() +
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
          legend.direction = "horizontal",
          legend.position= "bottom", 
          strip.text = element_text(size=18, face="bold"),
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(1.5,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=18, face="bold"),
          axis.title.x = element_text(size=24, face="bold"),
          axis.title.y = element_text(size=24, face="bold"),
          axis.text.x = element_text(face="bold", size=18),
          axis.text.y = element_text(size=20, face="bold"),
          plot.title = element_text(size=22, face="bold"))
  
  
  return(p)
  
}