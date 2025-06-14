plot_dose <- function (dose_plt, legend_title = "Animals") 
{
  ggplot(dose_plt, aes(x = dose, y = incub/24)) + 
    geom_hex(bins = 30, col = "white") + 
    scale_fill_viridis_c(option = "D", direction = -1, 
                         alpha = 0.4, limits = c(0, NA), name = legend_title) + 
    geom_smooth(method = "gam", 
                formula = y ~ s(x), 
                color = "black", 
                linewidth = 1.3) + 
    ylim(0, 15) + labs(title = " ", 
                       x = expression(Dose ~ (log[10] ~ copies/ml)), 
                       y = "Incubation Time (days)") + 
    theme_minimal() + 
    theme(plot.margin = unit(c(0.8, 1, 0.8, 1), "cm"), 
          legend.direction = "horizontal", 
          legend.position = "bottom", 
          strip.text = element_text(size = 14, face = "bold"),
          strip.background = element_blank(), 
          legend.key.size = unit(1, "line"), 
          legend.key.width = unit(3, "line"), 
          legend.text = element_text(size = 16,face = "bold"), 
          legend.title = element_text(size = 20,face = "bold"), 
          axis.title.x = element_text(size = 22, face = "bold"), 
          axis.title.y = element_text(size = 22, face = "bold"), 
          axis.text.x = element_text(size = 20, face = "bold", vjust = 0.5), 
          axis.text.y = element_text(size = 20, face = "bold"), 
          plot.title = element_text(size = 22, face = "bold"))
}