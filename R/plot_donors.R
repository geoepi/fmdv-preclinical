plot_donors <- function(df) {
  
  donor_long <- df %>%
    filter(group == "donor") %>%
    pivot_longer(cols = c(nasal, serum, score), names_to = "measure", values_to = "value")
  
  donor_long$value[donor_long$value == 45] <- 0
  
  donor_long$value2 <- ifelse(donor_long$measure == "score", donor_long$value * 2, donor_long$value)
  
  donor_long$measure <- factor(donor_long$measure,
                               levels = c("nasal", "serum", "score"),
                               labels = c("Nasal Swabs", "Serum", "Lesion Score"))
  
  custom_colors <- c("Nasal Swabs" = tol()[5], "Serum" = tol()[8], "Lesion Score" = tol()[2])
  custom_shapes <- c("Nasal Swabs" = 21, "Serum" = 24, "Lesion Score" = 25)
  
  p <- ggplot(donor_long, aes(x = hpe, y = value2, color = measure, fill = measure, group = measure)) +
    geom_rect(aes(xmin = 24, xmax = 48, ymin = -Inf, ymax = Inf),
              alpha = 0.2, color = NA, fill = "gray80", inherit.aes = FALSE) +
    geom_rect(aes(xmin = 48, xmax = 72, ymin = -Inf, ymax = Inf),
              alpha = 0.8, color = NA, fill = "gray85", inherit.aes = FALSE) +
    geom_rect(aes(xmin = 72, xmax = 96, ymin = -Inf, ymax = Inf),
              alpha = 0.2, color = NA, fill = "gray90", inherit.aes = FALSE) +
    geom_vline(xintercept = 0,
               linewidth = 1,
               linetype = "dotdash",
               col = "gray60") +
    annotate("text", x = 0, y = 9, label = "Inoculation (Day 0)", angle = 90, vjust = -0.5, hjust = 1) +
    annotate("text", x = 36, y = 10, label = "Group 1 Exp") +
    annotate("text", x = 60, y = 10, label = "Group 2 Exp") +
    annotate("text", x = 84, y = 10, label = "Group 3 Exp") +
    geom_point(aes(shape = measure, fill = measure, col = measure), size = 3) +
    geom_line(linewidth = 1) +
    geom_point(aes(shape = measure, fill = measure, col = measure), size = 3) +
    scale_x_continuous(breaks = seq(0, 125, 24), labels = seq(0, 125, 24), limits = c(0, 125)) +
    scale_y_continuous(limits = c(0, 10),
                       sec.axis = sec_axis(~ . / 2, name = "Lesion Score")) +
    xlab("Hours Post Exposure (HPE)") +
    ylab("FMDV RNA (log10 copies/ml)") +
    scale_color_manual(values = custom_colors, name = "Measure") +
    scale_fill_manual(values = custom_colors, name = "Measure") +
    scale_shape_manual(values = custom_shapes, name = "Measure") +
    facet_wrap(~animal, ncol = 1, scales = "free_y") +
    theme_minimal() +
    theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
          legend.direction = "horizontal",
          legend.position = "bottom",
          strip.text = element_text(size = 18, face = "bold"),
          strip.background = element_blank(),
          legend.key.size = unit(2, "line"),
          legend.key.width = unit(1.5, "line"),
          legend.text = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 18, face = "bold"),
          axis.title.x = element_text(size = 24, face = "bold"),
          axis.title.y = element_text(size = 24, face = "bold"),
          axis.text.x = element_text(face = "bold", size = 18),
          axis.text.y = element_text(size = 20, face = "bold"),
          plot.title = element_text(size = 22, face = "bold"))
  
  return(p)
}
