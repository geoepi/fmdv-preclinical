simulate_SEIIR <- function(seiir_model, distrib, initial_state, n_iterations = 1000, timesteps = 200) {
  
      times <- seq(0, timesteps, by = 1)

      # function to draw random parameters
      draw_parameters <- function(distrib) {
        list(
          beta = rlnorm(1, distrib$beta_meanlog, distrib$beta_sdlog), # S to E
          sigma = rlnorm(1, distrib$sigma_meanlog, distrib$sigma_sdlog), # E to I_sub
          lambda = rlnorm(1, distrib$lambda_meanlog, distrib$lambda_sdlog), # I_sub to I_clin
          gamma_clin = rlnorm(1, distrib$gamma_clin_meanlog, distrib$gamma_clin_sdlog), # removal rate
          N = distrib$N
        )
      }
      
      results_list <- vector("list", n_iterations)
      
      for (i in 1:n_iterations) {
        params <- draw_parameters(param_distributions)
        
        output <- ode(y = initial_state, times = times, func = seiir_model, parms = params)
        
        results_list[[i]] <- as.data.frame(output)
      }
      
      # combine all results
      all_results <- bind_rows(lapply(results_list, function(df) df %>% mutate(iteration = row_number())), .id = "iteration")
      
      # summary stats
      summarized_results <- all_results %>%
        gather(key = "compartment", value = "count", -time, -iteration) %>%
        group_by(time, compartment) %>%
        summarize(
          Q_0.025 = quantile(count, probs = 0.025),
          Q_0.25 = quantile(count, probs = 0.25),
          Q_0.5 = median(count),
          Q_0.75 = quantile(count, probs = 0.75),
          Q_0.975 = quantile(count, probs = 0.975)
        )
      
      return(summarized_results)
      
}