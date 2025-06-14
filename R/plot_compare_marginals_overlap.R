plot_compare_marginals_overlap <- function(samples_inc, samples_lat, x_max = 7, 
                                           xlabel = "Duration (Days)", ylabel = "Probability") {
  
  samples_lat$set <- "latent"
  samples_inc <- samples_inc %>%
    filter(time_vect <= max(samples_lat$time_vect)) %>%
    mutate(set = "incubation")
  
  overlap_df <- tibble(
    time_vect = samples_lat$time_vect,
    lat_med   = samples_lat$quant0.5,
    inc_med   = samples_inc$quant0.5
  ) %>%
    mutate(
      ymin = if_else(inc_med > lat_med, lat_med, NA_real_),
      ymax = if_else(inc_med > lat_med, inc_med, NA_real_),
      ymin_alt = if_else(lat_med > inc_med, inc_med, NA_real_),
      ymax_alt = if_else(lat_med > inc_med, lat_med, NA_real_)
    )
  
  
  
  x_positions <- c(0.6, 1.9, 4) 
  annotations <- c("Latent", "SubClinical", "Clinical")
  
  ggplot() +
    geom_ribbon(data = samples_lat, aes(x = time_vect, ymin = quant0.025, ymax = quant0.5),
                fill = "steelblue", alpha = 0.3) +
    geom_ribbon(data = samples_inc, aes(x = time_vect, ymin = quant0.5, ymax = quant0.975),
                fill = "gold", alpha = 0.3) +
    geom_ribbon(data = overlap_df, aes(x = time_vect, ymin = ymin, ymax = ymax),
                fill = "darkolivegreen", alpha = 0.3, inherit.aes = FALSE) +
    geom_line(data = samples_inc, aes(x = time_vect, y = quant0.5), color = "gray50", linewidth = 1) +
    geom_line(data = samples_lat, aes(x = time_vect, y = quant0.5), color = "gray50",
              linetype = "longdash", linewidth = 1) +
    geom_hline(yintercept = 0.5, linetype = "dotdash", linewidth = 0.6, color = "gray20") +
    #annotate("text", x = x_positions, y = rep(0.52, length(x_positions)),
    #         label = annotations, size = 6, fontface = "bold") +
    scale_x_continuous(breaks = seq(0, x_max, 1), labels = seq(0, x_max, 1), limits = c(0, x_max)) +
    xlab(xlabel) + ylab(ylabel) +
    theme_minimal() +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      legend.position = "none",
      strip.text = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 24, face = "bold"),
      axis.title.y = element_text(size = 24, face = "bold"),
      axis.text.x = element_text(size = 18, face = "bold"),
      axis.text.y = element_text(size = 20, face = "bold"),
      plot.title = element_text(size = 22, face = "bold")
    )
}
