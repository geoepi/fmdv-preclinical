simulate_SEIIR_intervention <- function(seiir_model, distrib, initial_state, 
                                         n_iterations = 1000, timesteps = 200, 
                                         detect = 1, vacc_effect = 0,
                                         immune_param = c(7, 1)) {
  
  times <- seq(0, timesteps, by = 1)
  
  draw_parameters <- function(distrib) {
    list(
      beta = rlnorm(1, distrib$beta_meanlog, distrib$beta_sdlog),
      sigma = rlnorm(1, distrib$sigma_meanlog, distrib$sigma_sdlog),
      lambda = rlnorm(1, distrib$lambda_meanlog, distrib$lambda_sdlog),
      gamma_clin = rlnorm(1, distrib$gamma_clin_meanlog, distrib$gamma_clin_sdlog),
      p_sub = rbeta(1, distrib$p_sub_shape1, distrib$p_sub_shape2),
      N = distrib$N
    )
  }
  
  event_function <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      reduction_amount <- as.numeric(state["S"]) * vacc_effect
      state["S"] <- as.numeric(state["S"]) - reduction_amount  
      state["R"] <- as.numeric(state["R"]) + reduction_amount  
      return(state)
    })
  }
  
  # trigger when clin_inc >= detection + delay until immunity
  immune_lag <- rnorm(1, immune_param[1], immune_param[2])
  
  detect <- detect + immune_lag
  
  root_function <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      clin_inc <- as.numeric(parameters["lambda"]) * as.numeric(state["I_sub"])
      return(clin_inc - detect)
    })
  }
  
  results_list <- vector("list", n_iterations)
  
  for (i in 1:n_iterations) {
    params <- draw_parameters(param_distributions)
    
    output <- ode(y = initial_state, times = times, func = seiir_model, parms = params,
                  events = list(func = event_function, root = TRUE),
                  rootfun = root_function)
    
    results_list[[i]] <- as.data.frame(output)
  }
  
  # Combine all results
  all_results <- do.call(rbind, lapply(results_list, function(df) df))
  all_results$iteration <- rep(1:n_iterations, each = nrow(results_list[[1]]))
  
  # Summary stats
  summary_results <- all_results %>%
    gather(key = "compartment", value = "count", -time, -iteration) %>%
    group_by(time, compartment) %>%
    summarize(
      Q_0.025 = quantile(count, probs = 0.025),
      Q_0.25 = quantile(count, probs = 0.25),
      Q_0.5 = median(count),
      Q_0.75 = quantile(count, probs = 0.75),
      Q_0.975 = quantile(count, probs = 0.975)
    )
  
  summarized_results <- list()
  summarized_results[["summary"]] <- as.data.frame(summary_results)
  summarized_results[["all_results"]] <- as.data.frame(all_results)
  
  return(summarized_results)
}