plot_seiir_dynamics <- function(summarized_results, 
                               ylimits = c(0, 22000),
                               legend_title = "Compartment",
                               xlab = " ", ylab = " ") {
  
  summarized_results$compartment <- factor(summarized_results$compartment, levels = c("S", "E", "I_sub", "I_clin", "R") )
  
  p <- ggplot(summarized_results, aes(time, Q_0.5, group=compartment, color=compartment)) +
  #geom_vline(xintercept = as_date("2015-01-01"),
  #           linetype = "longdash",
  #           linewidth=0.7,
  #           col="gray50") +
  #geom_bar(data=my_truth, aes(date, rescaled_count), stat="identity", alpha=0.3, 
  #         inherit.aes=FALSE, color="transparent", fill="gray50", width=1) +
  geom_ribbon(aes(ymin = Q_0.025, ymax = Q_0.975, group=compartment, fill=compartment), 
              size=0.01, color="transparent", alpha = 0.2) +
  geom_ribbon(aes(ymin = Q_0.25, ymax = Q_0.75, group=compartment, fill=compartment), 
              size=0.01, color="transparent", alpha = 0.4) +
  geom_line(linewdith = 1.0) +
  #scale_x_date(date_breaks = "45 day", date_labels = "%b %d") +
  #scale_y_continuous(sec.axis = sec_axis(~ . / scaling_factor, name = " "), limits = ylimits) +
  scale_colour_manual(values = c("gray60", "#1F78B4", "#B15928", "#E31A1C", "#33A02C")) +
  scale_fill_manual(values = c("gray75", "#A6CEE3", "#FFFF99","#FB9A99", "#B2DF8A")) + 
  labs(x = xlab, y = ylab, color = legend_title, fill = legend_title) +
  xlab(xlab) + 
  ylab(ylab) +
  theme_bw() +
  # facet_wrap(~set, ncol=1, scales = "free_y") +
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