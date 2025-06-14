plot_contact_groups <- function(df) {
  
  groups_long <- df %>%
    filter(group != "donor") %>%
    pivot_longer(cols = c(nasal, serum, score), names_to = "measure", values_to = "value")
  
  groups_long$value[groups_long$value == 45] = 0
  
  df_summary <- groups_long %>%
    group_by(group, dpe, measure) %>%
    summarize(
      mean_value = mean(value, na.rm = TRUE),
      se = sd(value, na.rm = TRUE) / sqrt(n())
    )
  
  df_summary <- df_summary %>%
    filter(is.na(mean_value) == FALSE)
  
  custom_colors <- c("Group 1" = tol()[5], "Group 2" = tol()[2], "Group 3" = tol()[6], "Group 4" = tol()[8])
  custom_shapes <- c("Group 1" = 21, "Group 2" = 24, "Group 3" = 21, "Group 4" = 25)
  
  df_summary$measure <- factor(df_summary$measure,
                               levels = c("nasal", "serum", "score"),
                               labels = c("Nasal Swabs", "Serum", "Score"))
  
  p <- ggplot(df_summary, aes(x = dpe*24, y = mean_value, color=group, fill=group, group=group)) +
    annotate("rect", xmin = 0, xmax = 24, ymin = -Inf, ymax = Inf,
             alpha = 0.5, color = NA, fill = "gray82") +
    #annotate("text", x = 12, y = 8, label = "Exposure", color = "gray30", alpha=0.5, size = 5, fontface = "bold") +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se, col=group), 
                  width = 12, position = position_dodge(0.02)) +
    geom_point(aes(shape = group, fill=group, col=group), size = 3.6) +
    geom_line(linewidth = 1) +
    geom_point(aes(shape = group, fill=group, col=group), size = 3.6) +
    scale_x_continuous(breaks = seq(0, 168, 24), labels= seq(0, 168, 24), limits = c(0,170)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
    xlab("Days Post Exposure (DPE)") + 
    ylab("FMDV RNA (log10 copies/ml)") +
    scale_color_manual(values = custom_colors, name = "Groups") +
    scale_fill_manual(values = custom_colors, name = "Groups") +
    scale_shape_manual(values = custom_shapes, name = "Groups") +
    facet_wrap(~measure, ncol=1, scales = "free_y") +
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
          plot.title = element_text(size=22, face="bold")
          ) +
    guides(
      color = guide_legend(override.aes = list(size = 4.2)),
      shape = guide_legend(override.aes = list(size = 4.2)),
      fill = guide_legend(override.aes = list(size = 4.2))
    )
  
  return(p)
}