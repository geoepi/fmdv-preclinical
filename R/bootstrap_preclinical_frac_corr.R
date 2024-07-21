bootstrap_preclinical_frac_corr <- function(group_means, n = 1000, m = 1000, ylims = c(0, 1)) {
  
  compute_bootstrap <- function(si_dist, incu_dist, type) {
    Y_succ <- numeric(m)
    
    for (j in 1:m) {
      si_samps <- sample(si_dist, n, replace = TRUE)
      incu_samps <- sample(incu_dist, n, replace = TRUE)
      
      if (type == "correlated") {
        si_samps <- sort(si_samps)
        incu_samps <- sort(incu_samps)
      } else if (type == "uncorrelated") {
        si_samps <- sort(si_samps)
        incu_samps <- sort(incu_samps, decreasing = TRUE)
      }
      
      preclin_count <- sum(si_samps < incu_samps)
      Y_succ[j] <- preclin_count / n
    }
    
    return(Y_succ)
  }
  
  results <- data.frame(
    iteration = integer(),
    mean_proportion = numeric(),
    lower_ci = numeric(),
    upper_ci = numeric(),
    method = character()
  )
  
  all_types <- c("random", "correlated", "uncorrelated")
  Y_succ_all <- list() 
  
  iter_grps <- length(unique(group_means$group))
  
  for(i in 1:iter_grps){
    set.seed(123)
    incu_dist <- rnorm(n, mean = group_means$mean_incu[i], sd = 2)
    si_dist <- rnorm(n, mean = group_means$si_mean[i], sd = 2)
    
    for (type in all_types) {
      Y_succ <- compute_bootstrap(si_dist, incu_dist, type)
      Y_succ_all[[paste0("Group", i, "_", type)]] <- Y_succ
      
      mean_proportion <- mean(Y_succ)
      ci_proportion <- quantile(Y_succ, probs = c(0.025, 0.975))
      
      results <- rbind(results, data.frame(
        iteration = i,
        mean_proportion = mean_proportion,
        lower_ci = ci_proportion[1],
        upper_ci = ci_proportion[2],
        method = type
      ))
    }
  }
  
  boxplot_data <- do.call(rbind, lapply(names(Y_succ_all), function(name) {
    parts <- strsplit(name, "_")[[1]]
    group <- as.numeric(gsub("Group", "", parts[1]))
    method <- parts[2]
    data.frame(
      Iteration = group,
      Proportion = Y_succ_all[[name]],
      Method = method
    )
  }))
  
  boxplot_data$group <- factor(paste0("Group ", boxplot_data$Iteration + 1))
  boxplot_data$Method <- factor(boxplot_data$Method, levels = all_types, labels = c("Random", "Correlated", "Uncorrelated"))
  
  violin_plot <- ggplot(boxplot_data, aes(x = group, y = Proportion)) +
    geom_violin(aes(fill = Method), alpha = 0.3) +
    stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), 
                 geom = "pointrange", color = "black") +
    ylim(ylims[1], ylims[2]) +
    facet_wrap(~ Method) +
    labs(title = "Fractional Pre-Clinical Transmission",
         x = " ",
         y = "Proportion Transmission") +
    theme_minimal() +
    theme(plot.margin = unit(c(1,0.75,1,0.75),"cm"),
          legend.direction = "horizontal",
          legend.position="none",
          strip.text = element_text(size=16, face="bold"),
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
  
  return(list(results = results, plot = violin_plot))
}