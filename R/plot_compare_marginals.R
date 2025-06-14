plot_compare_marginals <- function(samples_inc, samples_lat, x_max = 7, xlabel = "Duration (Days)", 
                                    ylabel = "Probability") {
  samples_lat$set <- "latent"
  
  samples_inc <- samples_inc %>%
    filter(time_vect <= max(samples_lat$time_vect)) %>%
    mutate(set = "incubation")
  
  comb_samps <- rbind(samples_inc, samples_lat)
  
  inter_curves <- samples_lat %>%
    select(time_vect, quant0.5) %>%
    mutate(inc_med = samples_inc$quant0.5)
  
  x_positions <- c(0.6, 1.9, 4) 
  annotations <- c("Latent", "PreClinical", "Clinical")
  
  p <- ggplot(comb_samps, aes(time_vect, quant0.5, group=set)) +
    geom_ribbon(data=inter_curves,
                aes(ymin=quant0.5, ymax=inc_med, group=NA), fill="steelblue", col=NA, alpha=0.3) +  
    geom_line(data=samples_inc,
              aes(time_vect, quant0.5), , linetype="solid", col="gray40", linewidth = 1) +
    geom_line(data=samples_lat,
              aes(time_vect, quant0.5), , linetype="longdash", col="gray40", linewidth = 1) +
    geom_hline(yintercept = 0.5,
               linetype = "dotdash",
               linewidth = 0.65,
               col = "gray30") +
    xlab(xlabel) + 
    ylab(ylabel) +
    scale_x_continuous(breaks= seq(0, x_max, 1), labels = seq(0, x_max, 1), limits = c(0, x_max)) +
    scale_fill_manual(values = c("orange")) +   
    theme_minimal() +
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
          legend.direction = "horizontal",
          legend.position= "none", 
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
          plot.title = element_text(size=22, face="bold")) +
     annotate("text", x = x_positions, y = rep(0.52, length(x_positions)),
              label = annotations, size = 6, fontface = "bold")
  
  return(p)
  
}











