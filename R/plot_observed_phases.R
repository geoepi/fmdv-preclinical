plot_observed_phases <- function(plot_data) {
  
  plot_data$period <- ifelse(plot_data$period == "uninfected", "Post-Exposure", plot_data$period)
  plot_data$period <- ifelse(plot_data$period == "infected", "Infected", plot_data$period)
  plot_data$period <- ifelse(plot_data$period == "clinical", "Clinical", plot_data$period)
  plot_data$period <- factor(plot_data$period, levels = c("Post-Exposure", "Infected", "Clinical")) 
  
  min_date <- min(as.Date(plot_data$start_date))
  
  exposure_data <- plot_data %>%
    filter(animal != "BR23-17" & animal != "BR23-18" & period == "Post-Exposure") %>%
    mutate(
      period = "Exposure",
      adjusted_end_date = as.Date(start_date) + 1
    )
  
  plot_data <- bind_rows(plot_data, exposure_data)

  plot_data$period <- factor(plot_data$period, levels = c("Exposure", "Post-Exposure", "Infected", "Clinical"))
  
  ggplot(data = plot_data, aes(y = animal, fill = period)) +
    geom_rect(aes(xmin = as.numeric(as.Date(start_date) - min_date),
                  xmax = as.numeric(as.Date(adjusted_end_date) - min_date),
                  ymin = as.numeric(as.factor(animal)) - 0.4,
                  ymax = as.numeric(as.factor(animal)) + 0.4),
              color = "gray30", linewidth = 0.15) +
    scale_x_continuous(breaks = seq(0, max(as.numeric(as.Date(plot_data$adjusted_end_date) - min_date)), by = 1)) +
    scale_fill_manual(values = c("Exposure" = "gray89", "Post-Exposure" = "#9ECAE1", "Infected" = "#4292C6", "Clinical" = "#08306B")) +
    labs(title = " ", x = "Experiment Day", y = "Animal ID", fill = "Phase") +
    annotate("text", x = 0.5, y = 4.5, label = "Group 1", angle = 90, color = "gray20", size = 6, fontface = "bold") +
    annotate("text", x = 1.5, y = 8.5, label = "Group 2", angle = 90, color = "gray20", size = 6, fontface = "bold") +
    annotate("text", x = 2.5, y = 12.5, label = "Group 3", angle = 90, color = "gray20", size = 6, fontface = "bold") +
    annotate("text", x = 3.5, y = 16.5, label = "Group 4", angle = 90, color = "gray20", size = 6, fontface = "bold") +
    annotate("text", x = 7, y = 1.5, label = "Donors", angle = 0, color = "gray20", size = 6, fontface = "bold") +
    theme_minimal() +
    theme(
      plot.margin = unit(c(1, 0.75, 1, 0.75), "cm"),
      panel.grid.major.x = element_line(color = "gray", linewidth = 0.5), 
      panel.grid.minor.x = element_blank(), 
      legend.direction = "horizontal",
      legend.position = "bottom",
      strip.text = element_text(size = 14, face = "bold"),
      strip.background = element_blank(),
      legend.key.size = unit(2, "line"),
      legend.key.width = unit(3, "line"),
      legend.text = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 22, face = "bold"),
      axis.text.x = element_text(face = "bold", size = 15, vjust = 0.5),
      axis.text.y = element_text(size = 18, face = "bold"),
      plot.title = element_text(size = 22, face = "bold")
    )
}
